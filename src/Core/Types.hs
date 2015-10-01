{- | The Core language defined -}
module Core.Types where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Control.Lens
import GHC.Generics (Generic)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Data.IndexedSet (IndexedSet, IndexKey(..), SplitKey(..))
import qualified Data.IndexedSet as I
import Core.Pretty

type Name = Text

instance IndexKey Name Name where

instance SplitKey Name Name where
  type WithoutKey Name Name = ()

class TempVar a where
  tempNum :: Int -> a

instance TempVar String where
  tempNum n = "a_" ++ show n

instance TempVar Text where
  tempNum n = "a_" <> T.pack (show n)

instance TempVar Int where
  tempNum = id

-- | Annotated data type.
data Ann' meta a = Ann meta a
                 deriving (Show, Eq, Generic)

type Ann meta tree = Ann' meta (tree meta)

type WrapAnn t var lit meta = Ann meta (t var lit)

instance (Pretty meta, Pretty a) => Pretty (Ann' meta a) where
  pretty (Ann meta a)
    -- FIXME: ugly hack
    | showP meta == "()" = pretty a
    | otherwise = PP.align $ PP.brackets (pretty meta) PP.<$$> pretty a

instance Complex a => Complex (Ann' meta a) where
  complex (Ann _ a) = complex a

metainfo :: Lens (Ann' meta1 a) (Ann' meta2 a) meta1 meta2
metainfo = lens (\(Ann m _) -> m) (\(Ann _ a) m -> Ann m a)

annval :: Lens (Ann' meta a1) (Ann' meta a2) a1 a2
annval = lens (\(Ann _ a) -> a) (\(Ann m _) a -> Ann m a)

type MetaAnn a meta = Ann meta a

class MetaTraversal f where
  metamap' :: Traversal (f meta1) (f meta2) meta1 meta2

metamap :: MetaTraversal f => Traversal (Ann meta1 f) (Ann meta2 f) meta1 meta2
metamap f (Ann meta a) = Ann <$> f meta <*> metamap' f a

clearAnn :: MetaTraversal f => Ann meta f -> Ann () f
clearAnn = metamap .~ ()

class VarTraversal f where
  varmap' :: Ord var2 => Traversal (f var1 lit meta) (f var2 lit meta) var1 var2

varmap :: (VarTraversal f, Ord var2) => Traversal (WrapAnn f var1 lit meta) (WrapAnn f var2 lit meta) var1 var2
varmap f (Ann meta a) = Ann meta <$> varmap' f a

instance IndexKey k a => IndexKey k (Ann' meta a) where
  toIndex (Ann _ a) = toIndex a

instance SplitKey k a => SplitKey k (Ann' meta a) where
  type WithoutKey k (Ann' meta a) = Ann' meta (WithoutKey k a)
  splitKey = iso (\(Ann m (view splitKey -> (k, a))) -> (k, Ann m a)) (\(k, Ann m a) -> Ann m $ (k, a) ^. from splitKey)

instance Functor (Ann' meta) where
  fmap f (Ann m a) = Ann m (f a)

data Kind' meta = Star
                | KFun (Kind meta) (Kind meta)
                deriving (Show, Eq, Generic)

instance MetaTraversal Kind' where
  metamap' _ Star = pure Star
  metamap' f (KFun a b) = KFun <$> metamap f a <*> metamap f b

instance Pretty meta => Pretty (Kind' meta) where
  pretty Star = "*"
  pretty (KFun a b) = br a PP.<+> "->" PP.<+> br b

instance Complex (Kind' meta) where
  complex Star = False
  complex (KFun _ _) = True

type Kind meta = Ann meta Kind'

data Type' var lit meta = TVar var
                        | TLit lit
                        | TFun
                        | TApp (Type var lit meta) (Type var lit meta)
                        deriving (Show, Eq, Generic)

instance MetaTraversal (Type' var lit) where
  metamap' _ (TVar v) = pure $ TVar v
  metamap' _ (TLit l) = pure $ TLit l
  metamap' _ TFun = pure TFun
  metamap' f (TApp a b) = TApp <$> metamap f a <*> metamap f b

instance (Pretty var, Pretty lit, Pretty meta) => Pretty (Type' var lit meta) where
  pretty (TVar n) = pretty n
  pretty (TLit n) = pretty n
  pretty TFun = "(->)"
  pretty (TApp a b) = br a PP.<+> br b

instance Complex (Type' var lit meta) where
  complex (TVar _) = False
  complex (TLit _) = False
  complex TFun = False
  complex (TApp _ _) = True

type Type var lit meta = WrapAnn Type' var lit meta

data TyCon' var lit meta = TyCon lit [Type var lit meta]
                         deriving (Show, Eq, Generic)

instance MetaTraversal (TyCon' var lit) where
  metamap' f (TyCon n ts) = TyCon n <$> (traverse.metamap) f ts

instance (Pretty var, Pretty lit, Pretty meta) => Pretty (TyCon' var lit meta) where
  pretty (TyCon n vs) = pretty n PP.<+> PP.hsep (map pretty vs)

type TyCon var lit meta = WrapAnn TyCon' var lit meta

-- FIXME: [TyCon var lit meta] -> IndexedSet lit (TyCon var lit meta)
data TyDecl' var lit meta = TyDecl lit [var] [TyCon var lit meta]
                              deriving (Show, Eq, Generic)

instance (Pretty var, Pretty lit, Pretty meta) => Pretty (TyDecl' var lit meta) where
  pretty (TyDecl name vars []) = "data" PP.<+> pretty name PP.<+> PP.hsep (map pretty vars)
  pretty (TyDecl name vars (h:t)) = pretty (TyDecl name vars [] :: TyDecl' var lit meta)
                                   PP.<+> PP.align (PP.vsep $ ("=" PP.<+> pretty h) : map (("|" PP.<+>) . pretty) t)

instance MetaTraversal (TyDecl' var lit) where
  metamap' f (TyDecl n ts cs) = TyDecl n ts <$> (traverse.metamap) f cs

type family NameKey a

type instance NameKey Name = Name

instance (NameKey lit ~ k, SplitKey k lit) => IndexKey k (TyDecl' var lit meta) where

data TyDeclD var lit meta = TyDeclD (WithoutKey (NameKey lit) lit) [var] [TyCon var lit meta]

instance MetaTraversal (TyDeclD var lit) where
  metamap' f (TyDeclD k ts cs) = TyDeclD k ts <$> (traverse.metamap) f cs

instance (NameKey lit ~ k, SplitKey k lit) => SplitKey k (TyDecl' var lit meta) where
  type WithoutKey k (TyDecl' var lit meta) = TyDeclD var lit meta
  splitKey = iso fwd back
    where fwd (TyDecl k a b) = (kk, TyDeclD kv a b)
            where (kk, kv) = k^.splitKey
          back (kk, TyDeclD kv a b) = TyDecl k a b
            where k = (kk, kv)^.from splitKey

type TyDecl var lit meta = WrapAnn TyDecl' var lit meta

data Pat' var lit meta = PVar var
                       | PCon lit [Pat var lit meta]
                       deriving (Show, Eq, Generic)

instance MetaTraversal (Pat' var lit) where
  metamap' _ (PVar n) = pure $ PVar n
  metamap' f (PCon n ps) = PCon n <$> (traverse.metamap) f ps

instance VarTraversal Pat' where
  varmap' f (PVar n) = PVar <$> f n
  varmap' f (PCon n ps) = PCon n <$> (traverse.varmap) f ps

instance (Pretty var, Pretty lit, Pretty meta) => Pretty (Pat' var lit meta) where
  pretty (PVar v) = pretty v
  pretty (PCon n ps) = PP.parens $ pretty n PP.<+> PP.hsep (map pretty ps)

type Pat var lit meta = WrapAnn Pat' var lit meta

type Alts var lit meta = [(Pat var lit meta, Expr var lit meta)]

-- | Generic scoped untyped lambda calculus with free variables' type 'var',
--   literals 'lit' and metadata 'meta'.
data Expr' var lit meta = Var var
                        | Lit lit
                        | Builtin lit
                        | Int Integer
                        | Abs var (Expr var lit meta)
                        | App (Expr var lit meta) (Expr var lit meta)
                        | Let (Map var (Expr var lit meta)) (Expr var lit meta)
                        | Case (Expr var lit meta) (Alts var lit meta)
                        deriving (Show, Eq, Generic)

instance MetaTraversal (Expr' var lit) where
  metamap' _ (Var n) = pure $ Var n
  metamap' _ (Lit n) = pure $ Lit n
  metamap' _ (Builtin n) = pure $ Builtin n
  metamap' _ (Int i) = pure $ Int i
  metamap' f (Abs n e) = Abs n <$> metamap f e
  metamap' f (App a b) = App <$> metamap f a <*> metamap f b
  metamap' f (Let ns b) = Let <$> traverse (metamap f) ns <*> metamap f b
  metamap' f (Case e alts) = Case <$> metamap f e <*> traverse (\(p, pe) -> (,) <$> metamap f p <*> metamap f pe) alts

instance VarTraversal Expr' where
  varmap' f (Var n) = Var <$> f n
  varmap' _ (Lit n) = pure $ Lit n
  varmap' _ (Builtin n) = pure $ Builtin n
  varmap' _ (Int i) = pure $ Int i
  varmap' f (Abs n e) = Abs <$> f n <*> varmap f e
  varmap' f (App a b) = App <$> varmap f a <*> varmap f b
  varmap' f (Let ns b) = Let <$> (M.fromList <$> traverse (\(n, e) -> (,) <$> f n <*> varmap f e) (M.toList ns)) <*> varmap f b
  varmap' f (Case e alts) = Case <$> varmap f e <*> traverse (\(p, pe) -> (,) <$> varmap f p <*> varmap f pe) alts

instance (Pretty var, Pretty lit, Pretty meta) => Pretty (Expr' var lit meta) where
  pretty (Var v) = pretty v
  pretty (Lit l) = pretty l
  pretty (Builtin n) = pretty n
  pretty (Int i) = pretty i
  pretty (Abs v e) = "λ" PP.<> pretty v PP.<> "." PP.</> pretty e
  pretty (App a b) = br a PP.<+> br b
  pretty (Let es a) = "let" PP.<+> PP.align (PP.vsep $ map (\(n, e) -> pretty n PP.<+> "=" PP.<+> pretty e) $ M.toList es) PP.<$> "in" PP.<+> pretty a
  pretty (Case e alts) = "case" PP.<+> pretty e PP.<+> "of" PP.<$> PP.nest 2 (PP.vsep $ map palt alts)
    where palt (pat, exp') = pretty pat PP.<+> "->" PP.<+> pretty exp'

instance Complex (Expr' var lit meta) where
  complex (Var _) = False
  complex (Lit _) = False
  complex (Builtin _) = False
  complex (Int _) = False
  complex (Abs _ _) = True
  complex (App _ _) = True
  complex (Let _ _) = True
  complex (Case _ _) = True

type Expr var lit meta = WrapAnn Expr' var lit meta

type Types var lit meta = IndexedSet (NameKey lit) (TyDecl var lit meta)

type NType meta = Type Name Name meta
type NTyCon meta = TyCon Name Name meta
type NTyDecl meta = TyDecl Name Name meta
type NTypes meta = Types Name Name meta

-- | Position in a file, used for errors reporting until the typecheck succeeds.
data Pos = Pos { line :: !Int
               , col :: !Int
               }
         deriving (Show, Eq)

instance Pretty Pos where
  pretty (Pos l c) = "Pos" PP.<+> pretty (l, c)

data Program var lit tvar tlit tmeta emeta =
  Program { progTypes :: Types tvar tlit tmeta
          , progExpr :: Expr var lit emeta
          }
  deriving (Show, Generic)

instance (Pretty var, Pretty lit, Pretty tvar, Pretty tlit, Pretty meta, Pretty tmeta) =>
         Pretty (Program var lit tvar tlit meta tmeta) where
  pretty des = PP.vsep (map pretty $ I.toList $ progTypes des) PP.<$> PP.empty PP.<$> pretty (progExpr des)

fun :: meta -> Type var lit meta -> Type var lit meta -> Type var lit meta
fun pos a b = Ann pos $ TApp (Ann pos $ TApp (Ann pos TFun) a) b

app :: meta -> Type var lit meta -> Type var lit meta -> Type var lit meta
app pos a b = Ann pos $ TApp a b

var :: meta -> var -> Type var lit meta
var pos = Ann pos . TVar

lit :: meta -> lit -> Type var lit meta
lit pos = Ann pos . TLit

star :: meta -> Type var () meta
star pos = lit pos ()
