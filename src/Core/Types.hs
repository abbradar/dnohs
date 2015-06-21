{- | The Core language defined -}
module Core.Types where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid
import Control.Lens
import Text.PrettyPrint.Leijen.Text (Pretty)

import Data.IndexedSet (IndexedSet, IndexKey(..), SplitKey(..))

type Name = Text

class TempVar a where
  tempNum :: Int -> a

instance TempVar Text where
  tempNum n = "a_" <> T.pack (show n)

newtype TyVar n = TyVar n
                deriving (Show, Eq, Ord, TempVar, Pretty)

instance Functor TyVar where
  fmap f (TyVar n) = TyVar (f n)

newtype TyLit n = TyLit n
                deriving (Show, Eq, Ord, TempVar, Pretty)

instance Functor TyLit where
  fmap f (TyLit n) = TyLit (f n)

newtype ValLit n = ValLit n
                 deriving (Show, Eq, Ord, TempVar, Pretty)

instance Functor ValLit where
  fmap f (ValLit n) = ValLit (f n)

newtype ValVar n = ValVar n
                 deriving (Show, Eq, Ord, TempVar, Pretty)

instance Functor ValVar where
  fmap f (ValVar n) = ValVar (f n)

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

type Ann meta tree = Ann' meta (tree meta)

makeLenses ''Ann'

type MetaAnn a meta = Ann meta a

class MetaFunctor f where
  metamap' :: (meta1 -> meta2) -> f meta1 -> f meta2

metamap :: MetaFunctor f => (meta1 -> meta2) -> Ann meta1 f -> Ann meta2 f
metamap f (Ann meta a) = Ann (f meta) (metamap' f a)

instance IndexKey k a => IndexKey k (Ann' meta a) where
  toIndex (Ann _ a) = toIndex a

instance SplitKey k a => SplitKey k (Ann' meta a) where
  type WithoutKey k (Ann' meta a) = Ann' meta (WithoutKey k a)
  splitKey = iso (\(Ann m (view splitKey -> (k, a))) -> (k, Ann m a)) (\(k, Ann m a) -> Ann m $ (k, a) ^. from splitKey)
  
instance Functor (Ann' meta) where
  fmap f (Ann m a) = Ann m (f a)

data Type' var lit meta = TVar (TyVar var)
                        | TLit (TyLit lit)
                        | TFun
                        | TApp (Type var lit meta) (Type var lit meta)
                        deriving (Show, Eq)

instance MetaFunctor (Type' var lit) where
  metamap' _ (TVar v) = TVar v
  metamap' _ (TLit l) = TLit l
  metamap' _ TFun = TFun
  metamap' f (TApp a b) = TApp (metamap f a) (metamap f b)

type Type var lit meta = Ann meta (Type' var lit)

data TyCon' var lit meta = TyCon (TyLit lit) [Type var lit meta]
                         deriving (Show, Eq)

instance MetaFunctor (TyCon' var lit) where
  metamap' f (TyCon n ts) = TyCon n (map (metamap f) ts)

type TyCon var lit meta = Ann meta (TyCon' var lit)

data TyDecl' var lit meta = TyDecl (TyLit lit) [TyVar var] [TyCon var lit meta]
                              deriving (Show, Eq)

instance MetaFunctor (TyDecl' var lit) where
  metamap' f (TyDecl n ts cs) = TyDecl n ts (map (metamap f) cs)

instance IndexKey (TyLit lit) (TyDecl' var lit meta) where

data TyDeclD var lit meta = TyDeclD [TyVar var] [TyCon var lit meta]

instance MetaFunctor (TyDeclD var lit) where
  metamap' f (TyDeclD ts cs) = TyDeclD ts (map (metamap f) cs)

instance SplitKey (TyLit lit) (TyDecl' var lit meta) where
  type WithoutKey (TyLit lit) (TyDecl' var lit meta) = TyDeclD var lit meta
  splitKey = iso (\(TyDecl k a b) -> (k, TyDeclD a b)) (\(k, TyDeclD a b) -> TyDecl k a b)

type TyDecl var lit meta = Ann meta (TyDecl' var lit)

data Pat' var lit meta = PVar (ValVar var)
                       | PCon (ValLit lit) [Pat var lit meta]
                       deriving (Show, Eq)

instance MetaFunctor (Pat' var lit) where
  metamap' _ (PVar n) = PVar n
  metamap' f (PCon n ps) = PCon n (map (metamap f) ps)

type Pat var lit meta = Ann meta (Pat' var lit)

type Alts var lit meta = [(Pat var lit meta, Expr var lit meta)]

-- | Generic scoped untyped lambda calculus with free variables' type 'var', --   literals 'lit' and metadata 'meta'.
data Expr' var lit meta = Var (ValVar var)
                        | Lit (ValLit lit)
                        | Abs (ValVar var) (Expr var lit meta)
                        | App (Expr var lit meta) (Expr var lit meta)
                        | Case (Expr var lit meta) (Alts var lit meta)
                        deriving (Show, Eq)

instance MetaFunctor (Expr' var lit) where
  metamap' _ (Var n) = Var n
  metamap' _ (Lit n) = Lit n
  metamap' f (Abs n e) = Abs n (metamap f e)
  metamap' f (App a b) = App (metamap f a) (metamap f b)
  metamap' f (Case e alts) = Case (metamap f e) (map (\(p, pe) -> (metamap f p, metamap f pe)) alts)

type Expr var lit meta = Ann meta (Expr' var lit)

type Types var lit meta = IndexedSet (TyLit lit) (TyDecl var lit meta)

type NType meta = Type Name Name meta
type NTyCon meta = TyCon Name Name meta
type NTyDecl meta = TyDecl Name Name meta
type NTypes meta = Types Name Name meta

-- | Position in a file, used for errors reporting until the typecheck succeeds.
data Pos = Pos { line :: !Int
               , col :: !Int
               }
         deriving (Show, Eq)

data Program var lit tvar tlit tmeta emeta =
  Program { progTypes :: IndexedSet (TyLit tlit) (TyDecl tvar tlit tmeta)
          , progExpr :: Expr var lit emeta
          }
  deriving (Show, Eq)

