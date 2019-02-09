{-# LANGUAGE PatternGuards #-}

module System.Linux.Ptrace (
  TracedProcess(..), pid,
  RemotePtr, castRemotePtr,

  traceProcess,
  continue,
  detach,

  peekBytes,
  pokeBytes,

  peek
  -- poke
) where

import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable hiding (peek, poke)
import System.Linux.Ptrace.Syscall
import System.Posix.Signals
import System.Posix.Types
import System.Posix.Waitpid
import System.IO.MMap

-- Note: We don't hide the constructor, because there exist ptrace()
-- options that allow you to automatically trace all subprocesses,
-- in which case the suer must be able to turn their PIDs (received
-- via waitpid) into `TraceProcess`es.

data TracedProcess = TracedProcess {
  pid :: CPid
}

-- | Attach to an existing process
traceProcess :: CPid -> IO TracedProcess
traceProcess pid = do
  ptrace_attach pid
  m <- waitpid pid []
  case m of
    Just (pid', Stopped sig) | pid == pid', sig == sigSTOP
      -> return $ TracedProcess pid
    _ -> error $ "traceProcess: waitpid returned " ++ show m

-- | Attach to a new process
--traceForkExec :: IO () -> FilePath -> [String] -> IO TracedProcess
--traceForkExec setup file argv = do
--  pid <- fork (setup >> ptrace_traceme >> execvp file argv)
--  waitpid pid
--  return $ TracedProcess pid

-- | Continue a process until it hits a signal
-- FIXME: return info about the signal
continue :: TracedProcess -> IO ()
continue TracedProcess { pid = pid } = do
  ptrace_cont pid Nothing
  m <- waitpid pid []
  case m of
    Just (pid', Stopped sig) | pid == pid' -> return ()
    _ -> error $ "traceProcess: waitpid returned " ++ show m

detach :: TracedProcess -> IO ()
detach proc = ptrace_detach (pid proc) Nothing

peek :: Storable a => TracedProcess -> Ptr a -> IO a
peek proc addr = do
  result <- peekBytes proc addr $ sizeOf $ valueOf addr
  let (ptr, off, size) = BS.toForeignPtr result
  withForeignPtr ptr (\p -> peekByteOff p off)
 where
  valueOf :: Ptr a -> a
  valueOf = undefined

-- FIXME: Is it more efficient to keep /proc/pid/mem open and read that?
--        Probably depends on the length of the read. Profile. I suspect that
--        3 words is the point at which /proc/pid/mem is faster (2 syscalls
--        rather than 3).
peekBytes :: TracedProcess -> Ptr a -> Int -> IO BS.ByteString
peekBytes _ _ 0 = return BS.empty
peekBytes proc addr_ size = (BS.take size . BS.drop extraBytes . joinWords) `fmap` mapM (ptrace_peekdata (pid proc)) readPtrs
 where
  addr = ptrToWordPtr addr_
  wordSize = fromIntegral $ sizeOf addr
  alignedAddr = addr .&. complement (wordSize - 1)
  extraBytes = fromIntegral $ addr - alignedAddr
  totalBytes = fromIntegral $ size + extraBytes
  readPtrs = map (wordPtrToPtr . fromIntegral) [alignedAddr, alignedAddr+wordSize .. alignedAddr+totalBytes-1]
  joinWords = BS.pack . (extractBytes =<<)
  -- Assuming little-endian :O Could use peekByteOff instead?
  extractBytes n = map (fromIntegral . (0xff .&.) . (n `shiftR`)) [0, 8 .. fromIntegral $ 8*wordSize-1]

-- FIXME: Is it more efficient to keep /proc/<...>/mem open and write to that?
--        Does the kernel even support that?
pokeBytes :: TracedProcess -> Ptr a -> BS.ByteString -> IO ()
pokeBytes proc addr_ bs = do
  s <- start
  e <- end
  doWrite (s `mappend` bs `mappend` e)
 where
  addr = ptrToWordPtr addr_
  size = BS.length bs
  wordSize = sizeOf addr
  alignedAddr = addr .&. complement (fromIntegral wordSize - 1)
  startBytes = fromIntegral $ addr - alignedAddr
  endBytes = -(size + startBytes) .&. complement (wordSize - 1)
  totalBytes = size + startBytes + endBytes
  start = peekBytes proc (wordPtrToPtr alignedAddr) startBytes
  end = peekBytes proc (wordPtrToPtr (alignedAddr + fromIntegral startBytes)) endBytes

  writePtrs = map (wordPtrToPtr . fromIntegral) [alignedAddr, alignedAddr+fromIntegral wordSize .. alignedAddr+fromIntegral totalBytes-1]
  splitWords = map extractWord . chunksOf wordSize
  -- Assuming little-endian :O Could use pokeByteOff instead?
  extractWord = BS.foldl' (\n w -> n `shiftL` 8 .|. fromIntegral w) 0
  doWrite = sequence_ . zipWith (ptrace_pokedata (pid proc)) writePtrs . splitWords

-- FIXME: does mmapping this file actually work?
unsafeMapBytes :: TracedProcess -> Ptr a -> Int -> IO BS.ByteString
unsafeMapBytes proc addr size = mmapFileByteString ("/proc/" ++ show (pid proc) ++ "/mem") $ Just (fromIntegral $ ptrToWordPtr addr, size)

unsafeMapBytesL :: TracedProcess -> RemotePtr a -> Int -> IO BSL.ByteString
unsafeMapBytesL proc addr size = mmapFileByteStringLazy ("/proc/" ++ show (pid proc) ++ "/mem") $ Just (fromIntegral addr, fromIntegral size)

chunksOf :: Int -> BS.ByteString -> [BS.ByteString]
chunksOf n bs | BS.null bs = []
              | otherwise = let (chunk, bs') = BS.splitAt n bs in chunk:chunksOf n bs'
