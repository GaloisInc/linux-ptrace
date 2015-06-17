{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Ptrace.X86_64Regs where

import Data.Word
import Foreign.Storable
import System.Linux.Ptrace.GenStruct

-- see user_regs_struct in <sys/user.h>

x86_64_regs = ["r15","r14","r13","r12","rbp","rbx","r11","r10","r9","r8","rax","rcx","rdx","rsi","rdi","orig_rax","rip","cs","eflags","rsp","ss","fs_base","gs_base","ds","es","fs","gs"]
-- Can't reuse x86_64_regs here. Damn those stage restrictions.
genStruct "X86_64Regs" ["r15","r14","r13","r12","rbp","rbx","r11","r10","r9","r8","rax","rcx","rdx","rsi","rdi","orig_rax","rip","cs","eflags","rsp","ss","fs_base","gs_base","ds","es","fs","gs"] [t|Word64|]
