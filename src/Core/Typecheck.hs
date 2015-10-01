module Core.Typecheck
       ( QName(..)
       , QType
       , QTyCon
       , QTypes
       , QExpr
       , QMeta
       , QPat
       , Renamed
       , typecheck
       ) where

import Control.Arrow (second)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.IndexedSet as I
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Lens
import qualified Text.PrettyPrint.Leijen.Text as PP
import System.IO.Unsafe
import Debug.Trace

import Core.Types
import Core.Monad
import Core.Pretty
import Core.Typed

type QType = Type QName Name Pos
type QTyCon = TyCon QName Name Pos
type QTyDecl = TyDecl QName Name Pos
type QTypes = Types QName Name Pos
type QExpr = Expr QName Name QMeta
type QMeta = (Pos, [QType])
type QPat = Pat QName Name QMeta
type Renamed = Program QName Name QName Name Pos QMeta

data Scheme' var lit meta = Forall (Set var) (Type var lit meta)
                          deriving (Show, Eq)

instance MetaTraversal (Scheme' var lit) where
  metamap' f (Forall vs t) = Forall vs <$> metamap f t

instance (Pretty var, Pretty lit, Pretty meta) => Pretty (Scheme' var lit meta) where
  pretty (Forall vars e) = "∀" PP.<+> PP.hsep (map pretty $ S.toList vars) PP.<+> "." PP.<+> pretty e

instance FreeVars (Scheme' QName lit meta) where
  ftv (Forall vs t) = ftv t `S.difference` vs

type Scheme var lit meta = Ann meta (Scheme' var lit)

type SubstType' lit meta = Type QName lit meta
type SubstScheme' lit meta = Scheme QName lit meta
type Subst' lit meta = Map QName (SubstType' lit meta)
type Constraint' lit meta = (SubstType' lit meta, SubstType' lit meta)

type SubstType lit = SubstType' lit Pos
type SubstScheme lit = SubstScheme' lit Pos
type Subst lit = Subst' lit Pos
type Constraint lit = Constraint' lit Pos

class Substitutable a where
  type SubstLit a
  type SubstMeta a

  apply :: Subst' (SubstLit a) (SubstMeta a) -> a -> a

instance Substitutable a => Substitutable [a] where
  type SubstLit [a] = SubstLit a
  type SubstMeta [a] = SubstMeta a

  apply = map . apply

instance Substitutable (Subst' lit meta) where
  type SubstLit (Subst' lit meta) = lit
  type SubstMeta (Subst' lit meta) = meta

  apply a b = M.map (apply b) a `M.union` M.map (apply a) b

instance (Substitutable a, Substitutable b, SubstLit a ~ SubstLit b, SubstMeta a ~ SubstMeta b) => Substitutable (a, b) where
  type SubstLit (a, b) = SubstLit a
  type SubstMeta (a, b) = SubstMeta a

  apply s (a, b) = (apply s a, apply s b)

instance Substitutable (TAnn a) where
  type SubstLit (TAnn a) = Name
  type SubstMeta (TAnn a) = ()

  apply s (TName t v) = TName (apply s t) v

instance Substitutable (SubstType' lit meta) where
  type SubstLit (SubstType' lit meta) = lit
  type SubstMeta (SubstType' lit meta) = meta

  apply s t@(Ann _ (TVar v)) = M.findWithDefault t v s
  apply _ t@(Ann _ (TLit _)) = t
  apply _ t@(Ann _ TFun) = t
  apply s (Ann pos (TApp a b)) = Ann pos $ TApp (apply s a) (apply s b)

instance Substitutable (SubstScheme' lit meta) where
  type SubstLit (SubstScheme' lit meta) = lit
  type SubstMeta (SubstScheme' lit meta) = meta

  apply s (Ann pos (Forall as t)) = Ann pos $ Forall as $ apply s' t
    where s' = foldr M.delete s as

instance Substitutable TContext where
  type SubstLit TContext = Name
  type SubstMeta TContext = Pos

  apply s = M.map (apply s)

instance FreeVars TContext where
  ftv = ftv . M.elems

occursCheck :: FreeVars a => QName -> a -> Bool
occursCheck n t = n `S.member` ftv t

showPA :: (MetaTraversal f, Pretty (f ())) => Ann meta f -> String
showPA = showP . clearAnn

unify :: (Pretty lit, Eq lit) => SubstType lit -> SubstType lit -> Compiler (Subst lit)
unify (Ann _ (TApp a b)) (Ann _ (TApp a' b')) = do
  s1 <- unify a a'
  s2 <- unify (apply s1 b) (apply s1 b')
  return $ apply s2 s1
unify (Ann _ (TVar a)) t = bind a t
unify t (Ann _ (TVar a)) = bind a t
unify (Ann _ (TLit a)) (Ann _ (TLit b)) | a == b = return M.empty
unify (Ann _ TFun) (Ann _ TFun) = return M.empty
unify a@(Ann pos _) b@(Ann _ _) = throwCError pos $ "Cannot unify types: " ++ showPA a ++ " and " ++ showPA b

unifyAll :: (Pretty lit, Eq lit) => [Constraint lit] -> Compiler (Subst lit)
unifyAll [] = return M.empty
unifyAll ((a, b):t) = do
  s1 <- unify a b
  s2 <- unifyAll (apply s1 t)
  --return $ apply s2 s1
  return $ apply s2 s1

bind :: QName -> SubstType lit -> Compiler (Subst lit)
bind n (Ann _ (TVar n')) | n == n' = return M.empty
bind n t@(Ann pos _) | occursCheck n t = throwCError pos "Infinite type"
                     | otherwise = return $ M.singleton n t

type Infer lit = WriterT [Constraint lit] Compiler

uni :: MonadWriter [Constraint lit] m => SubstType lit -> SubstType lit -> m ()
uni t1 t2 = tell [(t1, t2)]

type KType = SubstType ()
type KInfer = Infer ()

type KSubst = Map QName (Kind ())

unsafeQ :: Name -> QName
unsafeQ n = QName n 1

inferKType :: (Pos -> QName -> KType) -> (Pos -> Name -> KType) -> QType -> KInfer ()
inferKType getVar getLit topt@(Ann tpos _) = do
  r <- chk topt
  uni r $ star tpos

  where chk :: QType -> KInfer KType
        chk (Ann pos typ) = case typ of
          TApp a b -> do
            t1 <- chk a
            t2 <- chk b
            t <- var pos <$> genTemp
            uni t1 $ fun pos t2 t
            return t
          TVar v -> return $ getVar pos v
          TLit l -> return $ getLit pos l
          TFun -> return $ fun pos (star pos) (fun pos (star pos) (star pos))

ktypeToKind :: SubstType' () meta -> Kind ()
ktypeToKind (Ann _ p) = Ann () $ case p of
  -- by default
  TVar _ -> Star
  TLit _ -> Star
  TApp (Ann _ (TApp (Ann _ TFun) a)) b -> KFun (ktypeToKind a) (ktypeToKind b)
  _ -> error "ktypeToKind: invalid kind"

kindToKtype :: Pos -> Kind () -> KType
kindToKtype pos (Ann _ Star) = Ann pos $ TLit ()
kindToKtype pos (Ann _ (KFun a b)) = fun pos (kindToKtype pos a) (kindToKtype pos b)

inferKinds :: QTypes -> Compiler TTypes
inferKinds typs = do
  constraints <- execWriterT $ mapM inferKind $ I.toList typs
  ss <- unifyAll constraints
  return $ I.fromList $ map (applyKind $ M.map ktypeToKind ss) $ I.toList typs

  where applyKind :: KSubst -> QTyDecl -> TTyDecl
        applyKind subst (Ann _ (TyDecl name vars constrs)) =
          Ann () $ TyDecl (KName (subst M.! unsafeQ name) name) (map (\v -> KName (subst M.! v) v) vars) (map (applyConstr subst) constrs)

        applyConstr subst (Ann _ (TyCon name pars)) =
          Ann () $ TyCon (KName (Ann () Star) name) (map (applyPar subst) pars)

        applyPar subst = chk
          where chk (Ann _ p) = Ann () $ case p of
                  TVar v -> TVar $ KName (subst M.! v) v
                  TLit l -> TLit $ KName (subst M.! unsafeQ l) l
                  TFun -> TFun
                  TApp a b -> TApp (chk a) (chk b)

        inferKind :: QTyDecl -> KInfer ()
        inferKind (Ann pos (TyDecl name vars constrs)) = do
          let typ = var pos $ unsafeQ name
              ttyp = foldr (fun pos . var pos) (star pos) vars
          uni typ ttyp
          mapM_ (\(Ann _ (TyCon _ pars)) -> mapM_ inferPar pars) constrs

        inferPar :: QType -> KInfer ()
        inferPar = inferKType var (\p n -> var p $ unsafeQ n)

kindCheck :: TTypes -> QType -> Compiler ()
kindCheck typs typ = do
  constraints <- execWriterT $ inferKType' typ
  void $ unifyAll constraints
  where getLit pos name = kindToKtype pos kind
          where (Ann _ (TyDecl (KName kind _) _ _)) = typs I.! name
        inferKType' = inferKType var getLit

annsCheck :: TTypes -> QExpr -> Compiler ()
annsCheck typs expr = void (expr & metamap %%~ chk)
  where chk :: QMeta -> Compiler QMeta
        chk r@(_, anns) = do
          mapM_ (kindCheck typs) anns
          return r

type ConstrType = Type QName Name ()
type ConstrScheme = Scheme QName Name ()
type Constrs = Map Name Constr
type Constr = (ConstrScheme, [ConstrType])

buildConstrs :: QTypes -> Constrs
buildConstrs = M.fromList . concatMap getConstrs . I.toList
  where getConstrs :: QTyDecl -> [(Name, Constr)]
        getConstrs (Ann _ (TyDecl tname vars constrs)) = map mkcon constrs
          where ftype :: ConstrScheme
                ftype = Ann () $ Forall (S.fromList vars) $ foldl (\t a -> app () t (var () a)) (lit () tname) vars
                mkcon :: QTyCon -> (Name, Constr)
                mkcon (Ann _ (TyCon name pars)) = (name, (ftype, ptyps))
                  where ptyps :: [ConstrType]
                        ptyps = map clearAnn pars

type TInfer = ReaderT TContext (Infer Name)
type TSubst = Subst' Name ()
type TSubstType = SubstType Name
type TSubstScheme = SubstScheme Name
type TContext = Map QName TSubstScheme
type ITPat = Pat TQName TName Pos
type ITExpr = Expr TQName TName Pos

instVar :: QName -> TInfer QName
instVar (QName n _) = QName n <$> genTemp

instantiate :: TSubstScheme -> TInfer TSubstType
instantiate (Ann pos (Forall (S.toList -> as) t)) = do
    as' <- mapM (liftM (var pos) . instVar) as
    let s = M.fromList $ zip as as'
    return $ apply s t

generalize :: TContext -> TSubstType -> TSubstScheme
generalize env t@(Ann pos _)  = let a = Ann pos $ Forall as t in trace ("generalize: " ++ show a) a
  where as = ftv t S.\\ ftv env

-- TODO: a better inference engine:
-- * convert entire tree into ITExpr (with some initial type variables everywhere)
-- * iterate while there's something untraversed:
--   * collect constraints until we hit into a Let or a leaf
--   * solve constraints
--   * use new substitution to infer types of let bindings
--   * mark those Lets as traversed (keep set of any one variable names from Lets, skip them on encounter)

inferType :: Constrs -> QExpr -> Compiler TExpr
inferType constrs expr = do
  ((_, expr'), constraints) <- runWriterT (runReaderT (inferExpr expr) M.empty)
  let pr c = pretty (clearAnn c)
  let constraints' = unsafePerformIO $ do
        printDoc $ PP.vsep $ map (\(a, b) -> pretty a PP.<+> "::" PP.<+> pretty b) $ M.toList constrs
        printDoc $ PP.vsep $ map (\(a, b) -> pr a PP.<+> "~" PP.<+> pr b) constraints
        return constraints
  cs <- unifyAll constraints'
  let cs' = unsafePerformIO $ do
        printDoc $ PP.vsep $ map (\(a, b) -> pretty a PP.<+> "->" PP.<+> pr b) $ M.toList cs
        return cs
  return $ applyExpr (M.map clearAnn cs') expr'

    where getSubst :: TSubst -> TAnn a -> TAnn a
          getSubst subst (TName t n) = TName (apply subst t) n

          applyExpr :: TSubst -> TExpr -> TExpr
          applyExpr subst (Ann _ p) = Ann () $ case p of
            Var n -> Var (getSubst subst n)
            Lit n -> Lit (getSubst subst n)
            Builtin n -> Lit (getSubst subst n)
            Int i -> Int i
            Abs n e -> Abs (getSubst subst n) (applyExpr subst e)
            App e1 e2 -> App (applyExpr subst e1) (applyExpr subst e2)
            Let ns e -> Let (applyExpr subst <$> M.mapKeys (apply subst) ns) (applyExpr subst e)
            Case sc as -> Case (applyExpr subst sc) (map (\(pt, e) -> (applyPat subst pt, applyExpr subst e)) as)

          applyPat :: TSubst -> TPat -> TPat
          applyPat subst (Ann _ p) = Ann () $ case p of
            (PVar v) -> PVar $ getSubst subst v
            (PCon v ps) -> PCon (getSubst subst v) (map (applyPat subst) ps)

          inEnv :: QName -> TSubstScheme -> TInfer a -> TInfer a
          inEnv name s = local (M.insert name s)

          constrFun :: Pos -> Constr -> TInfer TSubstType
          constrFun pos (Ann _ (Forall vars ftype), ptyps) =
            instantiate $ Ann pos $ Forall vars (foldr (fun ()) ftype ptyps & metamap .~ pos)

          inferPat :: QPat -> TInfer (TContext, TSubstType, TPat)
          inferPat (Ann (pos, anns) p) = do
            r@(_, typ, _) <- case p of
              PVar v -> do
                let tv = var pos v
                return (M.singleton v $ Ann pos $ Forall S.empty tv, tv, Ann () $ PVar $ TName (clearAnn tv) v)
              PCon name pats -> do
                let (Ann _ (Forall vars ctyp), ts) = constrs M.! name
                -- want this in the library!
                let fromSetM f = liftM M.fromList . mapM (\n -> (n, ) <$> f n) . S.toList
                subst <- fromSetM (\n -> var () <$> instVar n) vars
                (ctxs, pats') <- liftM unzip $ forM (zip pats ts) $ \(pt, t) -> do
                  (ctx, typ, pt') <- inferPat pt
                  let t' = apply subst t & metamap .~ pos
                  uni typ t'
                  return (ctx, pt')
                let ctx = foldr M.union M.empty ctxs
                    ctyp' = apply subst ctyp & metamap .~ pos
                    name' = TName (clearAnn ctyp') name
                return (ctx, ctyp', Ann () $ PCon name' pats')
            mapM_ (uni typ) anns
            return r

          inferExpr :: QExpr -> TInfer (TSubstType, TExpr)
          inferExpr (Ann (pos, anns) expr') = do
            (typ, expr'') <- case expr' of
              Var x -> do
                env <- ask
                t <- instantiate $ env M.! x
                return (t, Var $ TName (clearAnn t) x)
              Lit l -> do
                let c = constrs M.! l
                t <- constrFun pos c
                return (t, Lit $ TName (clearAnn t) l)
              -- type info is contained in the annotation for builtin things
              Builtin n -> do
                t <- var pos <$> genTemp
                return (t, Builtin $ TName (clearAnn t) n)
              Int i -> do 
                t <- var pos <$> genTemp
                return (t, Int i)
              Abs x e -> do
                let tv = var pos x
                (t', e') <- inEnv x (Ann pos $ Forall S.empty tv) $ inferExpr e
                let t = fun pos tv t'
                return (t, Abs (TName (clearAnn tv) x) e')
              App e1 e2 -> do
                (t1, e1') <- inferExpr e1
                (t2, e2') <- inferExpr e2
                t <- var pos <$> genTemp
                uni t1 (fun pos t2 t)
                return (t, App e1' e2')
              Let ns e -> do
                let vars = M.mapWithKey (\n (Ann (lpos, _) _) -> var lpos n) ns
                env <- ask
                ns' <-
                  local (M.union $ fmap (Ann pos . Forall S.empty) vars) $
                  liftM M.fromList $ forM (M.toList ns) $ \(n, ne@(Ann (_, lanns) _)) -> do
                    let typ = vars M.! n
                    (nt, ne') <- inferExpr ne
                    mapM_ (uni typ) lanns
                    uni typ nt
                    return (TName (clearAnn typ) n, ne')
                -- FIXME: enable generalization back and implement engine properly instead (see above)
                --(t, e') <- local (M.union $ fmap (generalize env) vars) $ inferExpr e
                (t, e') <- local (M.union $ fmap (Ann pos . Forall S.empty) vars) $ inferExpr e
                return (t, Let ns' e')
              Case scr alts -> do
                (te, scr') <- inferExpr scr
                t <- var pos <$> genTemp
                alts' <- forM alts $ \(pat, e) -> do
                  (ctx, typ, pat') <- inferPat pat
                  uni te typ
                  (tp, e') <- local (M.union ctx) $ inferExpr e
                  uni tp t
                  return (pat', e')
                return (t, Case scr' alts')
            mapM_ (uni typ) anns
            return (typ, Ann () expr'')

-- FIXME: finish and use for better inference
splitLets :: QExpr -> QExpr
splitLets (Ann pos expr) = Ann pos $ case expr of
  Let ns e -> undefined
  Var v -> Var v
  Lit l -> Lit l
  Builtin n -> Builtin n
  Int i -> Int i
  Abs v e -> Abs v $ splitLets e
  App e1 e2 -> App (splitLets e1) (splitLets e2)
  Case e alts -> Case (splitLets e) (map (second splitLets) alts)

typecheck :: Renamed -> Compiler Typechecked
typecheck prog = do
  kinded <- inferKinds $ progTypes prog
  annsCheck kinded $ progExpr prog
  let constrs = buildConstrs $ progTypes prog
  typed <- inferType constrs $ progExpr prog
  return $ Program { progTypes = kinded
                   , progExpr = typed
                   }
