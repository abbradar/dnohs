{- | The Core language defined -}
module Core.Types where

import Data.Text (Text)
import Control.Lens

import Data.IndexedSet (IndexKey(..), SplitKey(..))

type Name = Text

-- | Bound variable with name and unique number to identify.
-- | The number is guaranteed to be unique for the whole tree.
-- | TODO: we can achieve faster Eq by only comparing Integers.
data Bound = Bound Name Integer
           deriving (Show, Eq)

-- | Bound variable or a free value of type 'var'
data Var var = VBound Bound
             | VFree var
             deriving (Show, Eq)

-- | Annotated data type.
data Ann' meta a = Ann { _metainfo :: meta
                       , _annval :: a
                       }
                 deriving (Show, Eq)

makeLenses ''Ann'

instance IndexKey k a => IndexKey k (Ann' meta a) where
  toIndex (Ann _ a) = toIndex a

instance SplitKey k a => SplitKey k (Ann' meta a) where
  type WithoutKey k (Ann' meta a) = Ann' meta (WithoutKey k a)
  splitKey = iso (\(Ann' m a) -> (m, a^.splitKey)) (\(m, a) -> Ann' m (a^.from splitKey))
  
instance Functor (Ann' meta) where
  fmap f (Ann m a) = Ann m (f a)

type Ann meta tree = Ann' meta (tree meta)

data TyName' var lit meta = TNVar var
                          | TNLit lit
                          deriving (Show, Eq)

type TyName meta var lit = Ann meta (TyName' var lit)

data Type' var lit meta = TApp (TyName meta var lit) [Type meta var lit]
                        | TFun (Type meta var lit) (Type meta var lit)
                        deriving (Show, Eq)

type Type meta var lit = Ann meta (Type' var lit)

data TyCon' var lit meta = TyCon lit [Type meta var lit]
                         deriving (Show, Eq)

type TyCon meta var lit = Ann meta (TyCon' var lit)

data TyDecl' var lit meta = TyDecl lit [var] [TyCon meta var lit]
                          deriving (Show, Eq, IndexKey)

instance SplitKey lit (TyDecl' var lit meta) where
  type WithoutKey lit (TyDecl' var lit meta) = ([var], [TyCon meta var lit])
  splitKey = iso (\(TyDecl k a b) -> (k, (a, b))) (\(k, (a, b)) -> TyDecl k a b)

type TyDecl meta var lit = Ann meta (TyDecl' var lit)

data Pat' var lit meta = PVar var
                       | PCon lit [Pat meta var lit]
                       deriving (Show, Eq)

type Pat meta var lit = Ann meta (Pat' var lit)

type Alts meta var lit = [(Pat meta var lit, Expr meta var lit)]

-- | Generic scoped untyped lambda calculus with free variables' type 'var', --   literals 'lit' and metadata 'meta'.
data Expr' var lit meta = Var var
                        | Lit lit
                        | Abs var (Expr meta var lit)
                        | App (Expr meta var lit) (Expr meta var lit)
                        | Case (Expr meta var lit) (Alts meta var lit)
                        deriving (Show, Eq)

type Expr meta var lit = Ann meta (Expr' var lit)

-- | Position in a file, used for errors reporting until the typecheck succeeds.
data Pos = Pos { line :: !Int
               , col :: !Int
               }
         deriving (Show, Eq)
