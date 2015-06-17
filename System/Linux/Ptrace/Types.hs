{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Linux.Ptrace.Types (Regs(..), X86Regs, X86_64Regs, Cuser_fpregs_struct, Csiginfo_t) where

-- Wrapping of various types.

import Data.Word
import Foreign.Storable
import System.Linux.Ptrace.X86Regs (X86Regs)
import System.Linux.Ptrace.X86_64Regs (X86_64Regs)

data Regs = X86 X86Regs | X86_64 X86_64Regs deriving Show

-- FIXME: see sys/user.h
newtype Cuser_fpregs_struct = CUFS Word deriving Storable

-- FIXME: see bits/siginfo.h
newtype Csiginfo_t = CSI Word deriving Storable
