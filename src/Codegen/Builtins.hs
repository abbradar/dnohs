module Codegen.Builtins where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.IndexedSet as I

import Core.Types

intType :: Name 
intType = "Int"

boolType :: Name
boolType = "Bool"

btrue :: Name
btrue = "True"

bfalse :: Name
bfalse = "False"

builtinTypes :: Types Name Name ()
builtinTypes =
  I.fromList
  [ an $ TyDecl intType [] []
  --, an $ TyDecl boolType []
  --  [ an $ TyCon btrue []
  --  , an $ TyCon bfalse []
  --  ]
  ]

  where an = Ann ()

--intEq :: Name
--intEq = "eq"

builtins :: Map Name (Type Name Name ())
builtins =
  M.fromList
  [ ("undefined", var () "a")
  , ("add", fun () tint (fun () tint tint))
  --, (intEq, fun () tint (fun () tint tbool))
  ]

  where tint = lit () intType
        tbool = lit () boolType
