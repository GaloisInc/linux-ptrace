{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

-- | Sadly, only the OS thread which performed the ptrace_attach is allowed
-- to mess with the traced process. This means that users of this module will
-- need to forkOS or runInBoundThread in order to get reliable behaviour.

module System.Linux.Ptrace.Syscall (
  RemotePtr(),
  castRemotePtr,
  Option(..),
  ptrace_traceme,
  ptrace_attach,
  ptrace_peektext, ptrace_peekdata, ptrace_peekuser,
  ptrace_poketext, ptrace_pokedata, ptrace_pokeuser,
  ptrace_cont, ptrace_syscall, ptrace_singlestep, ptrace_detach,
  ptrace_kill,
  ptrace_getregs,
  ptrace_setregs,
  --ptrace_getfpregs,
  --ptrace_setfpregs,
  ptrace_setoptions,
  ptrace_geteventmsg,
  --ptrace_getsiginfo,
  --ptrace_setsiginfo
) where

import Foreign
import Foreign.C
import Foreign.C.Types(CInt(..), CLong(..))
import GHC.Ptr(Ptr(..))
import Foreign.Ptr(WordPtr(..), Ptr(..), IntPtr(..))
import Data.Bits
import Data.List (foldl')
import Data.Maybe
import System.Linux.Ptrace.Types
import System.Posix.Signals (Signal, nullSignal)
import System.Posix.Types (CPid(..))
import System.Process

newtype RemotePtr a = RemotePtr WordPtr deriving (Eq, Ord, Show, Num, Bits, Storable, Enum, Real, Integral)
castRemotePtr (RemotePtr a) = RemotePtr a


type DataArg = WordPtr

foreign import ccall unsafe "ptrace" c_ptrace :: CInt -> CPid -> Ptr a -> Ptr b -> IO CLong


data Event = EventFork | EventVFork | EventClone | EventExec | EventVForkDone | EventExit deriving (Eq, Show)

event :: CLong -> Event
event 1 = EventFork
event 2 = EventVFork
event 3 = EventClone
event 4 = EventExec
event 5 = EventVForkDone
event 6 = EventExit
event n = error $ "ptrace: unexpected event code " ++ show n


handlePtraceResult :: IO CLong -> IO ()
handlePtraceResult = throwErrnoIfMinus1_ "ptrace"

throwErrnoIfSet :: IO a -> IO a
throwErrnoIfSet act = do
  resetErrno
  r <- act
  e <- getErrno
  if e /= eOK then throwErrno "ptrace" else return r


-- | Invoke the ptrace system call with various arguments.
ptrace4 n pid addr data_ = handlePtraceResult $ c_ptrace n pid addr data_
ptrace2 n pid = ptrace4 n pid (wordPtrToPtr 0) (wordPtrToPtr 0)
ptrace1 n = ptrace2 n 0

-- FIXME: better handling of EFAULT/EIO here (invalid read/write in other process's memory)

-- | Perform one of the PTRACE_PEEK* operations.
ptracePeek n pid addr = fromIntegral `fmap` (throwErrnoIfSet $ c_ptrace n pid addr $ wordPtrToPtr 0)
-- | Perform one of the PTRACE_POKE* operations.
ptracePoke n pid addr val = ptrace4 n pid addr (wordPtrToPtr $ fromIntegral val)

-- | Perform one of the PTRACE_GET* operations.
ptraceGet n pid = alloca (\ptr -> ptrace4 n pid (wordPtrToPtr 0) ptr >> peek ptr)
-- | Perform one of the PTRACE_SET* operations.
ptraceSet n pid val = alloca (\ptr -> poke ptr val >> ptrace4 n pid (wordPtrToPtr 0) ptr)

-- | Resume a traced process.
ptraceResume n pid sig = ptrace4 n pid (wordPtrToPtr 0) (wordPtrToPtr $ maybe 0 fromIntegral sig)


-- | Attach the parent process to this process.
ptrace_traceme :: IO ()
ptrace_traceme = ptrace1 0

-- | Attach to a process.
-- FIXME: handle EPERM. return IO Bool?
ptrace_attach :: CPid -> IO ()
ptrace_attach = ptrace2 16

-- | Read a word from the traced process.
ptrace_peektext, ptrace_peekdata, ptrace_peekuser ::
  CPid -> Ptr Word -> IO Word
ptrace_peektext = ptracePeek 1
ptrace_peekdata = ptracePeek 2
ptrace_peekuser = ptracePeek 3

-- | Write a word to the traced process.
ptrace_poketext, ptrace_pokedata, ptrace_pokeuser ::
  CPid -> Ptr Word -> Word -> IO ()
ptrace_poketext = ptracePoke 4
ptrace_pokedata = ptracePoke 5
-- FIXME: EBUSY can come out when setting debug registers
ptrace_pokeuser = ptracePoke 6

-- | Continue the traced process, possibly with a signal.
ptrace_cont, ptrace_syscall, ptrace_singlestep, ptrace_detach ::
  CPid -> Maybe Signal -> IO ()
ptrace_cont = ptraceResume 7
ptrace_syscall = ptraceResume 24
ptrace_singlestep = ptraceResume 9
ptrace_detach = ptraceResume 17

-- | Send the traced process a SIGKILL.
ptrace_kill :: CPid -> IO ()
ptrace_kill pid = ptrace2 8 pid


ptrace_getregs :: CPid -> IO Regs
ptrace_getregs pid | sizeOf (0 :: RemotePtr ()) == 4 = X86 `fmap` ptraceGet 12 pid
                   | sizeOf (0 :: RemotePtr ()) == 8 = X86_64 `fmap` ptraceGet 12 pid

-- Rely on caller to pass in right sort of registers.
ptrace_setregs :: CPid -> Regs -> IO ()
ptrace_setregs pid (X86 regs) = ptraceSet 13 pid regs
ptrace_setregs pid (X86_64 regs) = ptraceSet 13 pid regs


ptrace_getfpregs :: CPid -> IO Cuser_fpregs_struct
ptrace_getfpregs pid = ptraceGet 14 pid
ptrace_setfpregs :: CPid -> Cuser_fpregs_struct -> IO ()
ptrace_setfpregs pid regs = ptraceSet 15 pid regs


-- x86 only. On x86_64, getfpregs returns this stuff.
--ptrace_getfpxregs pid = ptraceGet 18 pid
--ptrace_setfpxregs pid regs = ptraceSet 19 pid regs


data Option = TraceSysGood | TraceFork | TraceVFork | TraceClone | TraceExec | TraceVForkDone | TraceExit

optionCode :: Option -> DataArg
optionCode TraceSysGood = 0x01
optionCode TraceFork = 0x02
optionCode TraceVFork = 0x04
optionCode TraceClone = 0x08
optionCode TraceExec = 0x10
optionCode TraceVForkDone = 0x20
optionCode TraceExit = 0x40

optionsCode :: [Option] -> DataArg
optionsCode = foldl' (.|.) 0 . map optionCode

ptrace_setoptions :: CPid -> [Option] -> IO ()
ptrace_setoptions pid opts = ptrace4 0x4200 pid (wordPtrToPtr 0) (wordPtrToPtr $ optionsCode opts)


ptrace_geteventmsg :: CPid -> IO CULong
ptrace_geteventmsg pid = ptraceGet 0x4201 pid


ptrace_getsiginfo :: CPid -> IO Csiginfo_t
ptrace_getsiginfo pid = ptraceGet 0x4202 pid
ptrace_setsiginfo :: CPid -> Csiginfo_t -> IO ()
ptrace_setsiginfo pid siginfo = ptraceSet 0x4203 pid siginfo

-- undocumented!

-- On x86 or for x86 processes on x86_64, get/set the TLS area.
-- ptrace_set_thread_area, ptrace_get_thread_area

-- On x86_64, get/set the TLS area
-- ptrace_arch_prctl "works just like arch_prctl except that the arguments are reversed"
