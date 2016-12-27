{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Ptrace.X86Regs where

import Data.Word
import Foreign.Storable
import System.Linux.Ptrace.GenStruct

-- see user_regs_struct in <sys/user.h>

x86_regs :: [String]
x86_regs = ["ebx","ecx","edx","esi","edi","ebp","eax","xds","xes","xfs","xgs","orig_eax","eip","xcs","eflags","esp","xss"]

genStruct "X86Regs" ["ebx","ecx","edx","esi","edi","ebp","eax","xds","xes","xfs","xgs","orig_eax","eip","xcs","eflags","esp","xss"] [t|Word32|]
