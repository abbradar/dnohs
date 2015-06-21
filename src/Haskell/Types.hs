module Haskell.Types where

import Control.Lens
import Core.Types

import Data.IndexedSet (IndexKey(..), SplitKey(..))

type HsTyName meta = NTyName meta
type HsType meta = NType meta
type HsTyCon meta = NTyCon meta
type HsTyDecl meta = NTyDecl meta

data HsPat' meta = HPVar (ValVar Name)
                 | HPWildCard
                 | HPCon (ValLit Name) [HsPat meta]
                 deriving (Show, Eq)

type HsPat meta = Ann meta HsPat'

data HsLet' meta = LAnn (ValVar Name) (HsType meta)
                 | LBind (ValVar Name) [HsPat meta] (HsExpr meta)
                 deriving (Show, Eq)

instance IndexKey (ValVar Name) (HsLet' meta)

instance SplitKey (ValVar Name) (HsLet' meta) where
  type WithoutKey (ValVar Name) (HsLet' meta) = Either (HsType meta) ([HsPat meta], (HsExpr meta))
  splitKey = iso back fwd
    where back (LAnn n a) = (n, Left a)
          back (LBind n a b) = (n, Right (a, b))
          fwd (n, Left a) = LAnn n a
          fwd (n, Right (a, b)) = LBind n a b

type HsLet meta = Ann meta HsLet'

type HsAlts meta = [(HsPat meta, HsExpr meta)]

data HsExpr' meta = HLet [HsLet meta] (HsExpr meta)
                  | HCase (HsExpr meta) (HsAlts meta)
                  | HVar (ValVar Name)
                  | HLit (ValLit Name)
                  | HApp (HsExpr meta) (HsExpr meta)
                  | HTyAnn (HsType meta) (HsExpr meta)
                  deriving (Show, Eq)

type HsExpr meta = Ann meta HsExpr'

data HsTop meta = TELet (HsLet meta)
                | TEData (HsTyDecl meta)
                deriving (Show, Eq)

type HsTopsP = [HsTop Pos]

makePrisms ''HsTop

makePrisms ''HsLet'
