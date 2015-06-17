{-# LANGUAGE TemplateHaskell #-}

module System.Linux.Ptrace.GenStruct where

import Control.Monad
import Foreign
import Language.Haskell.TH

genStruct :: String -> [String] -> Q Type -> Q [Dec]
genStruct name ctors elemType = do
  let name' = mkName name
  elemType' <- elemType
  varsAndTypes <- mapM (\n -> varStrictType (mkName n) (strictType notStrict elemType)) ctors
  let typeDecl = DataD [{-context-}] name' [{-tyvars-}] [RecC name' varsAndTypes] [{-deriving-}''Show]
  -- Could evaluate this now, but what happens if we're cross-compiling? Is CInt the target's size, or ours?
  let elemSize = [|sizeOf (undefined :: $(elemType))|]

  -- This exposes at least two GHC bugs:
  -- 1) It's rejected because GHC thinks $(conT name') is a type variable
  -- 2) The error message reverses the order of member definitions
  --storableInst <-
  --  [d|instance Storable $(conT name') where
  --       sizeOf _ = $(litP . integerL $ length ctors) * $(elemSize)
  --       alignment _ = alignment (undefined :: $(elemType))
  --       peek p = foldl (\e k -> [| $e `ap` peekByteOff p (k * $(elemSize)) |]) [|return $(conE name')|] [0..length ctors-1]
  --  |]

  -- This exposes another GHC bug:
  -- 3) We can't capture Storable in a type quotation since it's a class name.
  --storableInst <- instanceD (cxt []) [t|Storable $(conT name')|] ...

  -- Work around instanceD's nasty interface
  let fixDecs :: Q [Dec] -> Q [DecQ]
      fixDecs decs = (fmap.fmap) return decs

  -- Eek, can't substitute this below: TH lifting is not referentially transparent
  let numCtors = length ctors
  
  storableInst : [] <- [d| instance Storable $(return $ ConT name') where
                              sizeOf _ = numCtors * $(elemSize)
                              alignment _ = alignment (undefined :: $(elemType))
                              peek p = $(foldl (\e k -> [| $(e) `ap` peekByteOff p (k * $(elemSize)) |]) [|return $(conE name')|] [0..length ctors-1])
                              poke p v = sequence_ $(listE $ map (\(n,c) -> [| pokeByteOff p (n * $(elemSize)) ($(varE (mkName c)) v) |]) $ zip [0::Int ..] ctors)
                        |]

  return [typeDecl, storableInst]
