module System.Linux.Ptrace.X86_64FPRegs where

import Data.Word
import Foreign.Storable

data X86_64FPRegs = X86_64FPRegs {
  cwd :: Word16,
  swd :: Word16,
  ftw :: Word16,
  fop :: Word16,
  fp_rip :: Word64,
  rdp :: Word64,
  mxcsr :: Word32,
  mxcsr_mask :: Word32,
  -- FIXME: the FPU registers aren't actually 128  bits - where are they
  -- actually stored in this?
  st0 :: (Word16, Word64),
  st1 :: (Word16, Word64),
  st2 :: (Word16, Word64),
  st3 :: (Word16, Word64),
  st4 :: (Word16, Word64),
  st5 :: (Word16, Word64),
  st6 :: (Word16, Word64),
  st7 :: (Word16, Word64),
  xmm0 :: (Word64, Word64),
  xmm1 :: (Word64, Word64),
  xmm2 :: (Word64, Word64),
  xmm3 :: (Word64, Word64),
  xmm4 :: (Word64, Word64),
  xmm5 :: (Word64, Word64),
  xmm6 :: (Word64, Word64),
  xmm7 :: (Word64, Word64),
  xmm8 :: (Word64, Word64),
  xmm9 :: (Word64, Word64),
  xmm10 :: (Word64, Word64),
  xmm11 :: (Word64, Word64),
  xmm12 :: (Word64, Word64),
  xmm13 :: (Word64, Word64),
  xmm14 :: (Word64, Word64),
  xmm15 :: (Word64, Word64)
} deriving Show

instance Storable X86_64FPRegs where
  sizeOf _ = 512
  alignment _ = 64
  peek p = do 
    cwd' <- peekByteOff p 0
    swd' <- peekByteOff p 2
    ftw' <- peekByteOff p 4
    fop' <- peekByteOff p 6
    rip' <- peekByteOff p 8
    rdp' <- peekByteOff p 16
    mxcsr' <- peekByteOff p 24
    mxcsr_mask' <- peekByteOff p 28
    st0_high <- peekByteOff p 40
    st0_low <-  peekByteOff p 32
    st1_high <- peekByteOff p 56
    st1_low <-  peekByteOff p 48
    st2_high <- peekByteOff p 72
    st2_low <-  peekByteOff p 64
    st3_high <- peekByteOff p 88
    st3_low <-  peekByteOff p 80
    st4_high <- peekByteOff p 104
    st4_low <-  peekByteOff p 96
    st5_high <- peekByteOff p 120
    st5_low <-  peekByteOff p 112
    st6_high <- peekByteOff p 136
    st6_low <-  peekByteOff p 124
    st7_high <- peekByteOff p 152
    st7_low <-  peekByteOff p 144
    xmm0_high <- peekByteOff p 168
    xmm0_low <-  peekByteOff p 160
    xmm1_high <- peekByteOff p 184
    xmm1_low <-  peekByteOff p 176
    xmm2_high <- peekByteOff p 200
    xmm2_low <-  peekByteOff p 192
    xmm3_high <- peekByteOff p 216
    xmm3_low <-  peekByteOff p 208
    xmm4_high <- peekByteOff p 232
    xmm4_low <-  peekByteOff p 224
    xmm5_high <- peekByteOff p 248
    xmm5_low <-  peekByteOff p 240
    xmm6_high <- peekByteOff p 264
    xmm6_low <-  peekByteOff p 256
    xmm7_high <- peekByteOff p 280
    xmm7_low <-  peekByteOff p 272
    xmm8_high <- peekByteOff p 296
    xmm8_low <-  peekByteOff p 288
    xmm9_high <- peekByteOff p 312
    xmm9_low <-  peekByteOff p 304
    xmm10_high <- peekByteOff p 328
    xmm10_low <-  peekByteOff p 320
    xmm11_high <- peekByteOff p 344
    xmm11_low <-  peekByteOff p 336
    xmm12_high <- peekByteOff p 360
    xmm12_low <-  peekByteOff p 352
    xmm13_high <- peekByteOff p 376
    xmm13_low <-  peekByteOff p 368
    xmm14_high <- peekByteOff p 392
    xmm14_low <-  peekByteOff p 384
    xmm15_high <- peekByteOff p 408
    xmm15_low <- peekByteOff p 400
    return $ X86_64FPRegs {
      cwd = cwd',
      swd = swd',
      ftw = ftw',
      fop = fop',
      fp_rip = rip',
      rdp = rdp',
      mxcsr = mxcsr',
      mxcsr_mask = mxcsr_mask',
      -- FIXME: the FPU registers aren't actually 128  bits - where are they
      -- actually stored in this?

      st0 = (st0_high, st0_low),
      st1 = (st1_high, st1_low),
      st2 = (st2_high, st2_low),
      st3 = (st3_high, st3_low),
      st4 = (st4_high, st4_low),
      st5 = (st5_high, st5_low),
      st6 = (st6_high, st6_low),
      st7 = (st7_high, st7_low),
      xmm0 = (xmm0_high, xmm0_low),
      xmm1 = (xmm1_high, xmm1_low),
      xmm2 = (xmm2_high, xmm2_low),
      xmm3 = (xmm3_high, xmm3_low),
      xmm4 = (xmm4_high, xmm4_low),
      xmm5 = (xmm5_high, xmm5_low),
      xmm6 = (xmm6_high, xmm6_low),
      xmm7 = (xmm7_high, xmm7_low),
      xmm8 = (xmm8_high, xmm8_low),
      xmm9 = (xmm9_high, xmm9_low),
      xmm10 = (xmm10_high, xmm10_low),
      xmm11 = (xmm11_high, xmm11_low),
      xmm12 = (xmm12_high, xmm12_low),
      xmm13 = (xmm13_high, xmm13_low),
      xmm14 = (xmm14_high, xmm14_low),
      xmm15 = (xmm15_high, xmm15_low)
    }
    
  poke p v = do
    pokeByteOff p 0 $ cwd v
    pokeByteOff p 2 $ swd v 
    pokeByteOff p 4 $ ftw v 
    pokeByteOff p 6 $ fop v 
    pokeByteOff p 8 $ fp_rip v 
    pokeByteOff p 16 $ rdp v 
    pokeByteOff p 24 $ mxcsr v 
    pokeByteOff p 28 $ mxcsr_mask v 
    -- FIXME: the FPU registers aren't actually 128  bits - where are they
    -- actually stored in this?
    case st0 v
      of (high, low) -> do
          pokeByteOff p 40 high
          pokeByteOff p 32 low
    case st1 v
      of (high, low) -> do
          pokeByteOff p 56 high
          pokeByteOff p 48 low
    case st2 v
      of (high, low) -> do
          pokeByteOff p 72 high
          pokeByteOff p 64 low
    case st3 v
      of (high, low) -> do
          pokeByteOff p 88 high
          pokeByteOff p 80 low
    case st4 v
      of (high, low) -> do
          pokeByteOff p 96 low
    case st5 v
      of (high, low) -> do
          pokeByteOff p 120 high
          pokeByteOff p 112 low
    case st6 v
      of (high, low) -> do
          pokeByteOff p 136 high
          pokeByteOff p 124 low
    case st7 v
      of (high, low) -> do
          pokeByteOff p 152 high
          pokeByteOff p 144 low
    case xmm0 v
      of (high, low) -> do
          pokeByteOff p 168 high
          pokeByteOff p 160 low
    case xmm1 v
      of (high, low) -> do
          pokeByteOff p 184 high
          pokeByteOff p 176 low
    case xmm2 v
      of (high, low) -> do
          pokeByteOff p 200 high
          pokeByteOff p 192 low
    case xmm3 v
      of (high, low) -> do
          pokeByteOff p 216 high
          pokeByteOff p 208 low
    case xmm4 v
      of (high, low) -> do
          pokeByteOff p 232 high
          pokeByteOff p 224 low
    case xmm5 v
      of (high, low) -> do
          pokeByteOff p 248 high
          pokeByteOff p 240 low
    case xmm6 v
      of (high, low) -> do
          pokeByteOff p 264 high
          pokeByteOff p 256 low
    case xmm7 v
      of (high, low) -> do
          pokeByteOff p 280 high
          pokeByteOff p 272 low
    case xmm8 v
      of (high, low) -> do
          pokeByteOff p 296 high
          pokeByteOff p 288 low
    case xmm9 v
      of (high, low) -> do
          pokeByteOff p 312 high
          pokeByteOff p 304 low
    case xmm10 v
      of (high, low) -> do
          pokeByteOff p 328 high
          pokeByteOff p 320 low
    case xmm11 v
      of (high, low) -> do
          pokeByteOff p 344 high
          pokeByteOff p 336 low
    case xmm12 v
      of (high, low) -> do
          pokeByteOff p 360 high
          pokeByteOff p 352 low
    case xmm13 v
      of (high, low) -> do
          pokeByteOff p 376 high
          pokeByteOff p 368 low
    case xmm14 v
      of (high, low) -> do
          pokeByteOff p 392 high
          pokeByteOff p 384 low
    case xmm15 v
      of (high, low) -> do
          pokeByteOff p 408 high
          pokeByteOff p 400 low
