module Codegen.Generate
       ( generateLLVM
       ) where

import Prelude hiding (Ordering(..))
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Trans.Class
import Control.Monad.Writer
import Control.Monad.Reader hiding (local)
import Control.Lens hiding (assign, op)
import LLVM.General.AST
import qualified LLVM.General.AST.Constant as C
import LLVM.General.AST.Type (i1, i8, ptr)
import LLVM.General.AST.Global
import qualified LLVM.General.AST.Global as G
import LLVM.General.AST.IntegerPredicate
import Debug.Trace

import qualified Data.IndexedSet as I
import Core.Types (TempVar(..), Ann'(..), Pat'(..), Expr'(..), TyDecl'(..), TyCon'(..), Program(..))
import Core.Typed
import Core.Unique
import Core.Pretty (showP)
import Codegen.Utils

data Constr = Constr { conId :: Integer
                     , conArgs :: Integer
                     }

type Constrs = Map Text Constr

type Generator = ReaderT Constrs (Writer [Definition])
type DefGenerator = UniqueT Generator
type BlockGenerator = WriterT Instructions DefGenerator

tellDef :: MonadWriter [Definition] m => Global -> m ()
tellDef global = tell [GlobalDefinition global]

instance TempVar Name where
  tempNum = UnName . fromIntegral

qname' :: QName -> String
qname' (QName n i) = T.unpack n ++ "_" ++ show i

qname :: QName -> Name
qname = Name . qname'

cname' :: Text -> String
cname' = T.unpack

cname :: Text -> Name
cname = Name . cname'

allocName :: Name
allocName = Name "malloc"

allocType :: Type
allocType = FunctionType { resultType = voidptr
                         , argumentTypes = [int]
                         , isVarArg = False
                         }
printfName :: Name
printfName = Name "printf"

printfType :: Type
printfType = FunctionType { resultType = voidptr
                          , argumentTypes = [ptr i8]
                          , isVarArg = True
                          }

forceName :: Name
forceName = Name "force"

force' :: Name -> BlockGenerator Name
force' op = do
  resP <- genTemp
  res <- genTemp
  resTest <- genTemp
  let typ = FunctionType { resultType = VoidType
                         , argumentTypes = [ptr thunk]
                         , isVarArg = False
                         }
      op' = local (ptr thunk) op
  tell [ Do $ call' (globalRef typ forceName) [op']
       , resP := getElementPtr' op' [istr 0]
       , res := load' (local (ptr i8) resP)
       , resTest := icmp' EQ (local i8 res) (constant $ intv8 (-1))
       ]
  return resTest

undefinedName :: Name
undefinedName = Name "undefined"

setUndefined :: DefGenerator BasicBlock
setUndefined = do
  blockId <- genTemp
  let typ = FunctionType { resultType = VoidType
                         , argumentTypes = [ptr thunk]
                         , isVarArg = False
                         }
      cmds = [ Do $ call' (globalRef typ undefinedName) [local (ptr thunk) thunkArg]
             ]
  return $ retBlock blockId cmds

-- see here for builtins calling convention
builtin :: Text -> Name -> Instruction
builtin bname op = call' (globalRef typ $ cname bname) [local (ptr thunk) op]
  where typ = FunctionType { resultType = VoidType
                           , argumentTypes = [ptr thunk]
                           , isVarArg = False
                           }

thunkName :: Name
thunkName = Name "Thunk"

thunk :: Type
thunk = NamedTypeReference thunkName

thunkDef :: Type
thunkDef = StructureType { isPacked = False
                         , elementTypes =
                           [ i8 -- 0: not evaluated, -1: bottom, 1+: constructor number
                           , voidptr -- 0: closure, 1+: object
                           ]
                         }

thunkArg :: Name
thunkArg = Name "thunk"

thunkPar :: Name -> Parameter
thunkPar n = Parameter (ptr thunk) n []

closureArg :: Name
closureArg = Name "closure"

argArg :: Name
argArg = Name "arg"

-- arguments: resulting thunk, function closure, argument thunk (possibly null for non-functions)
evaluator :: Type
evaluator = FunctionType { resultType = VoidType
                         , argumentTypes = [ptr thunk, ptr closure, ptr thunk]
                         , isVarArg = False
                         }

closureName :: Name
closureName = Name "Closure"

closure :: Type
closure = NamedTypeReference closureName

thunks :: Type
thunks = ArrayType 0 (ptr thunk)

closureDef :: Type
closureDef = StructureType { isPacked = False
                           , elementTypes =
                             [ ptr evaluator
                             , thunks
                             ]
                           }
-- this doesn't check if the thunk is forced!
callFun :: Name -> Name -> Operand -> BlockGenerator ()
callFun funThunk resultThunk argThunk = do
  closureV <- getThunkVal closure funThunk
  evaluatorP <- genTemp
  evaluatorV <- genTemp
  tell [ evaluatorP := getElementPtr' (local (ptr closure) closureV) [istr 0]
       , evaluatorV := load' (local (ptr (ptr evaluator)) evaluatorP)
       , Do $ call' (local (ptr evaluator) evaluatorV)
         [ local (ptr thunk) resultThunk
         , local (ptr closure) closureV
         , argThunk
         ]
       ]

alloc :: Type -> C.Constant -> BlockGenerator Name
alloc typ sz = do
  objId' <- genTemp
  objId <- genTemp
  tell [ objId' := call' (globalRef allocType allocName) [constant sz]
       , objId := bitcast' (local voidptr objId') (ptr typ)
       ]
  return objId

alloc' :: Type -> BlockGenerator Name
alloc' typ = alloc typ (sizeof' typ 1)

getThunkVal :: Type -> Name -> BlockGenerator Name
getThunkVal typ th = do
  objIdP' <- genTemp
  objIdP <- genTemp
  objId <- genTemp
  tell [ objIdP' := getElementPtr' (local (ptr thunk) th) [istr 1]
       , objIdP := bitcast' (local voidptr objIdP') (ptr (ptr typ))
       , objId := load' (local (ptr (ptr typ)) objIdP)
       ]
  return objId

debugPrint :: String -> Int -> Generator ([Name] -> BlockGenerator ())
debugPrint prefix num = do
  let dname = Name $ "debug_print_" ++ prefix
      vals = intercalate " " $ replicate num "%lli"
      str = map (intv8 . fromIntegral . fromEnum) (prefix ++ ": " ++ vals ++ "\n\0")
      typ = ArrayType (fromIntegral $ length str) i8
      arg = ConstantOperand $ C.BitCast (C.GlobalReference (ptr typ) dname) (ptr i8)
  tell [ GlobalDefinition globalVariableDefaults { name = dname
                                                 , G.type' = typ
                                                 , initializer = Just $ C.Array i8 str
                                                 }
       ]

  return $ \names -> do
    when (length names /= num) $ fail "debugPrint: invalid arguments number"
    args <- forM names $ \n -> do
      chk <- force' n
      intP <- getThunkVal int n
      good <- genTemp
      magicVal <- genTemp
      intV <- genTemp
      resVal <- genTemp
      res <- genTemp
      -- FIXME: probably can be simplified
      tell [ good := select' (local i1 chk) (constant $ intv 0) (constant $ intv 1)
           , magicVal := select' (local i1 chk) (constant $ intv 444) (constant $ intv 0)
           , intV := load' (local (ptr int) intP)
           , resVal := mul' (local int good) (local int intV)
           , res := add' (local int resVal) (local int magicVal)
           ]
      return res
    tell [ Do $ call' (globalRef printfType printfName) (arg : map (local int) args) ]
    return ()

builtinDefinitions :: Generator ()
builtinDefinitions = do
  tell [ TypeDefinition thunkName (Just thunkDef)
       , TypeDefinition closureName (Just closureDef)
         -- `malloc`
       , GlobalDefinition functionDefaults { returnType = voidptr
                                           , name = allocName
                                           , parameters = ([Parameter int (Name "size") []], False)
                                           , basicBlocks = []
                                           }
         -- `printf`
       , GlobalDefinition functionDefaults { returnType = voidptr
                                           , name = printfName
                                           , parameters = ([Parameter (ptr i8) (Name "format") []], True)
                                           , basicBlocks = []
                                           }
       ]

  -- constructors
  constrs <- ask
  iforM_ constrs $ \n info -> runUniqueT $ do
    let vars = take (fromIntegral $ conArgs info) $ map (\x -> "arg_" ++ show x) [(1 :: Int)..]

        build :: [String] -> String -> Set Name -> DefGenerator [BasicBlock]
        build (h:t) aname args = genAbs' h aname args $ build t
        build [] aname _ = do
          blockId <- genTemp
          cmds <- execWriterT $ do
            storage <- alloc thunks $ sizeof' (ptr thunk) (conArgs info)
            forM_ (zip [0..] vars) $ \(i, ai) -> do
              argpos <- genTemp
              tell [ argpos := getElementPtrUnsafe' (local (ptr thunks) storage) [istr i]
                   , Do $ store' (local (ptr (ptr thunk)) argpos) (local (ptr thunk) $ Name ai)
                   ]
            updateThunk thunkArg (conId info) (local (ptr thunks) storage)
            return ()
          return [retBlock blockId cmds]

    blocks <- build vars (cname' n) S.empty
    let cdef = functionDefaults { returnType = VoidType
                                , name = cname n
                                , parameters = ([thunkPar thunkArg], False)
                                , basicBlocks = blocks
                                }
    tellDef cdef

  -- `force` -- force a thunk
  runUniqueT $ do
    blockId <- genTemp
    statusP <- genTemp
    status <- genTemp
    statusB <- genTemp
    forceId <- genTemp
    forceCmds <- execWriterT $ callFun thunkArg thunkArg (constant $ C.Null $ ptr thunk)
    dumbId <- genTemp
    let checkCmds = [ statusP := getElementPtr' (local (ptr thunk) thunkArg) [istr 0]
                    , status := load' (local (ptr i8) statusP)
                    , statusB := icmp' EQ (local i8 status) (constant $ intv8 0)
                    ]
        checkTerm = Do $ CondBr (local i1 statusB) forceId dumbId []
        checkBlock = BasicBlock blockId checkCmds checkTerm
        forceBlock = retBlock forceId forceCmds
        dumbBlock = retBlock dumbId []
        forceDef = functionDefaults { returnType = VoidType
                                    , name = forceName
                                    , parameters = ([thunkPar thunkArg], False)
                                    , basicBlocks = [checkBlock, forceBlock, dumbBlock]
                                    }
    tellDef forceDef

  -- `undefined` -- bottom
  runUniqueT $ do
    blockId <- genTemp
    cmds <- execWriterT $ updateThunk thunkArg (-1) (constant $ C.Null voidptr)
    let undefinedDef = functionDefaults { returnType = VoidType
                                        , name = undefinedName
                                        , parameters = ([thunkPar thunkArg], False)
                                        , basicBlocks = [retBlock blockId cmds]
                                        }
    tellDef undefinedDef

  -- `seq` -- force computations
  runUniqueT $ do
    let x = "a"
        y = "b"
    blocks <- genAbs' x "seq" S.empty $ genAbs'' y $ \_ _ -> do
      let getInt :: Name -> Name -> DefGenerator (Name, Name, [BasicBlock])
          getInt startId n = do
            (loadId, fblocks) <- force startId n
            (nR, loadCmds) <- runWriterT $ do
              nV <- getThunkVal int n
              nR <- genTemp
              tell [ nR := load' (local (ptr int) nV) ]
              return nR
            nextId <- genTemp
            let loadBlock = brBlock loadId loadCmds nextId
            return (nR, nextId, fblocks ++ [loadBlock])

      startId <- genTemp
      (forceId, f1blocks) <- force startId (Name x)
      (finId, f2blocks) <- force forceId (Name y)
      tmp <- genTemp
      let finCmds = [ tmp := load' (local (ptr thunk) (Name y))
                    , Do $ store' (local (ptr thunk) thunkArg) (local thunk tmp)
                    ]
          finBlock = retBlock finId finCmds

      return $ f1blocks ++ f2blocks ++ [finBlock]
    let seqDef = functionDefaults { returnType = VoidType
                                  , name = Name "seq"
                                  , parameters = ([thunkPar thunkArg], False)
                                  , basicBlocks = blocks
                                  }
    tellDef seqDef

  -- `add` -- add two ints
  runUniqueT $ do
    let x = "x"
        y = "y"
    blocks <- genAbs' x "add" S.empty $ genAbs'' y $ \_ _ -> do
      let getInt :: Name -> Name -> DefGenerator (Name, Name, [BasicBlock])
          getInt startId n = do
            (loadId, fblocks) <- force startId n
            (nR, loadCmds) <- runWriterT $ do
              nV <- getThunkVal int n
              nR <- genTemp
              tell [ nR := load' (local (ptr int) nV) ]
              return nR
            nextId <- genTemp
            let loadBlock = brBlock loadId loadCmds nextId
            return (nR, nextId, fblocks ++ [loadBlock])

      getxId <- genTemp
      (xR, getyId, getxBlocks) <- getInt getxId (Name x)
      (yR, compId, getyBlocks) <- getInt getyId (Name y)
      compCmds <- execWriterT $ do
        storage <- alloc' int
        res <- genTemp
        tell [ res := add' (local int xR) (local int yR)
             , Do $ store' (local (ptr int) storage) (local int res)
             ]
        updateThunk thunkArg 1 $ local (ptr int) storage
      let compBlock = retBlock compId compCmds
      return $ getxBlocks ++ getyBlocks ++ [compBlock]
    let addDef = functionDefaults { returnType = VoidType
                                  , name = Name "add"
                                  , parameters = ([thunkPar thunkArg], False)
                                  , basicBlocks = blocks
                                  }
    tellDef addDef

genEvaluator :: String -> Set Name -> (String -> Set Name -> DefGenerator [BasicBlock]) -> DefGenerator Name
genEvaluator ename vars' comp = do
  let ename' = Name $ ename ++ "_eval"
      vars = S.toAscList vars'
  blockId <- genTemp
  mains@((BasicBlock mainId _ _):_) <- comp ename vars'
  cmds <- execWriterT $ forM_ (zip [0..] vars) $ \(i, n) -> do
    t <- genTemp
    tell [ t := getElementPtrUnsafe' (local (ptr closure) closureArg) [istr 1, istr i]
         , n := load' (local (ptr (ptr thunk)) t)
         ]
  let prep = brBlock blockId cmds mainId
      args = [ thunkPar thunkArg
             , Parameter (ptr closure) closureArg []
             , thunkPar argArg
             ]
  let evalDef = functionDefaults { returnType = VoidType
                                 , name = ename'
                                 , parameters = (args, False)
                                 , basicBlocks = prep : mains
                                 }
  
  tellDef evalDef
  return ename'

force :: Name -> Name -> DefGenerator (Name, [BasicBlock])
force checkId op = do
  (resTest, checkCmds) <- runWriterT $ force' op
  failBlock@(BasicBlock failId _ _) <- setUndefined
  goodId <- genTemp
  let checkTerm = Do $ CondBr (local i1 resTest) failId goodId []
      check = BasicBlock checkId checkCmds checkTerm
  return (goodId, [check, failBlock])

genClosure :: String -> Set Name -> (String -> Set Name -> DefGenerator [BasicBlock]) -> BlockGenerator Name
genClosure clname vars' comp = do
  let vars = S.toAscList vars'
  newClosure <- alloc closure $ sizeof closure [istr' 1, istr' $ fromIntegral $ S.size vars']
  evaluatorP <- genTemp
  evaluatorF <- lift $ genEvaluator clname vars' comp

  tell [ evaluatorP := getElementPtr' (local (ptr closure) newClosure) [istr 0]
       , Do $ store' (local (ptr (ptr evaluator)) evaluatorP) (globalRef (ptr evaluator) evaluatorF)
       ]
  forM_ (zip [0..] vars) $ \(i, n) -> do
    t <- genTemp
    tell [ t := getElementPtrUnsafe' (local (ptr closure) newClosure) [istr 1, istr i]
         , Do $ store' (local (ptr (ptr thunk)) t) (local (ptr thunk) n)
         ]
  return newClosure

-- Build curried function.
genAbs :: String -> String -> Set Name -> (String -> Set Name -> DefGenerator [BasicBlock]) -> BlockGenerator ()
genAbs arg name' vars' comp = do
  let arg' = Name arg
  fname <- genClosure (name' ++ "_" ++ arg) (S.delete arg' vars') $ \clname vars -> do
    blockId <- genTemp
    mains@((BasicBlock mainId _ _):_) <- comp clname (S.insert arg' vars)
    let cmds = [ arg' := assign (ptr thunk) argArg ]
        block = brBlock blockId cmds mainId
    return $ block : mains
  updateThunk thunkArg 1 $ local (ptr closure) fname

-- convenience wrapper for chaining
genAbs' :: String -> String -> Set Name -> (String -> Set Name -> DefGenerator [BasicBlock]) -> DefGenerator [BasicBlock]
genAbs' arg aname vars comp = do
  blockId <- genTemp
  cmds <- execWriterT $ genAbs arg aname vars comp
  return [retBlock blockId cmds]

genAbs'' :: String -> (String -> Set Name -> DefGenerator [BasicBlock]) -> String -> Set Name -> DefGenerator [BasicBlock]
genAbs'' arg comp aname vars = genAbs' arg aname vars comp

genAlts :: String -> TAlts -> DefGenerator [BasicBlock]
genAlts aname alts = arec $ zip [(0 :: Int)..] alts
  where checkArgs :: Name -> Name -> [TPat] -> DefGenerator (Name, Name -> [BasicBlock])
        checkArgs startId scr pats = chk startId $ zip [0..] pats
          where chk blockId [] = return (blockId, \_ -> [])
                chk blockId ((i, s):t) = do
                  curScrP <- genTemp
                  curScr <- genTemp
                  nextId <- genTemp
                  let cmds = [ curScrP := getElementPtrUnsafe' (local (ptr thunks) scr) [istr i]
                             , curScr := load' (local (ptr (ptr thunk)) curScrP)
                             ]
                  let block = brBlock blockId cmds nextId
                  (goodId, blocks) <- checkPat nextId curScr s
                  (lastId, cblocks) <- chk goodId t
                  return (lastId, \badId -> [block] ++ blocks badId ++ cblocks badId)

        checkPat :: Name -> Name -> TPat -> DefGenerator (Name, Name -> [BasicBlock])
        checkPat startId scr (Ann _ pat) = chk pat
          where chk (PVar (TName _ n)) = do
                  goodId <- genTemp
                  let cmds = [ qname n := assign (ptr thunk) scr ]
                  return (goodId, \_ -> [brBlock startId cmds goodId])
                chk (PCon (TName _ n) ps) = do
                  (testId, fblocks) <- force startId scr
                  constrs <- ask
                  let coninfo = constrs M.! n
                  ((constrTest, dat), testCmds) <- runWriterT $ do
                    dat <- getThunkVal thunks scr
                    constrP <- genTemp
                    constr <- genTemp
                    constrTest <- genTemp
                    tell [ constrP := getElementPtr' (local (ptr thunk) scr) [istr 0]
                         , constr := load' (local (ptr i8) constrP)
                         , constrTest := icmp' EQ (local i8 constr) (constant $ intv8 $ conId coninfo)
                         ]
                    return (constrTest, dat)
                  goodId <- genTemp
                  let testTerm badId = Do $ CondBr (local i1 constrTest) goodId badId []
                      testBlock badId = BasicBlock testId testCmds (testTerm badId)
                  (foundId, blocks) <- checkArgs goodId dat ps
                  return (foundId, \badId -> fblocks ++ [testBlock badId] ++ blocks badId)

        arec [] = pure <$> setUndefined
        arec ((i, (p, e)):t) = do
          checkId <- genTemp
          (winId, cblocks) <- checkPat checkId (Name $ aname ++ "_s") p
          bblocks <- genExpr winId (aname ++ "_b" ++ show i) e
          fblocks@((BasicBlock failId _ _):_) <- arec t
          return $ cblocks failId ++ bblocks ++ fblocks

genExpr :: Name -> String -> TExpr -> DefGenerator [BasicBlock]
genExpr startId ename (Ann _ expr) = gen expr
  where callBuiltin n = do
          let cmds = [ Do $ builtin n thunkArg ]
          return [retBlock startId cmds]

        gen (Var (TName _ v)) = do
          let var = qname v
          (blockId, fblocks) <- force startId var
          tmp <- genTemp
          let cmds = [ tmp := load' (local (ptr thunk) var)
                     , Do $ store' (local (ptr thunk) thunkArg) (local thunk tmp)
                     ]
          return $ fblocks ++ [retBlock blockId cmds]
        gen (Lit (TName _ l)) = callBuiltin l
        gen (Builtin (TName _ b)) = callBuiltin b
        gen (Int i) = do
          cmds <- execWriterT $ do
            storage <- alloc' int
            tell [ Do $ store' (local (ptr int) storage) (constant $ intv i)
                 ]
            updateThunk thunkArg 1 $ local (ptr int) storage
          return [retBlock startId cmds]
        gen (Abs (TName _ x) e) = do
          cmds <- execWriterT $ genAbs (qname' x) ename (S.map qname $ ftv e) $ \ename' _ -> do
            nextId <- genTemp
            genExpr nextId ename' e
          return [retBlock startId cmds]
        gen (App f e) = do
          let fthunk = ename ++ "_f"
              fthname = Name fthunk
          fallocCmds <- execWriterT $ createThunk fthunk f
          forceId <- genTemp
          let fallocBlock = brBlock startId fallocCmds forceId
          (runId, fblocks) <- force forceId fthname
          let athunk = ename ++ "_a"
          runCmds <- execWriterT $ do
            createThunk athunk e
            callFun fthname thunkArg (local (ptr thunk) $ Name athunk)
          let runBlock = retBlock runId runCmds
          return $ [fallocBlock] ++ fblocks ++ [runBlock]
        gen (Let vs e) = do
          cmds <- execWriterT $ do
            iforM_ vs $ \(TName _ n) _ -> do
              storage <- alloc' thunk
              tell [ qname n := assign (ptr thunk) storage ]
            iforM_ vs $ \(TName _ n) v -> genThunk (qname' n) v
          exprId <- genTemp
          blocks@((BasicBlock nextId _ _):_) <- genExpr exprId (ename ++ "_l") e
          let block = brBlock startId cmds nextId
          return $ block:blocks
        gen (Case e alts) = do
          let sthunk = ename ++ "_s"
          startCmds <- execWriterT $ createThunk sthunk e
          blocks@((BasicBlock nextId _ _):_) <- genAlts ename alts
          let start = brBlock startId startCmds nextId
          return $ start:blocks

updateThunk :: Name -> Integer -> Operand -> BlockGenerator ()
updateThunk thname constr val' = do
  (status :: Name) <- genTemp
  closureP <- genTemp
  val <- genTemp
  let thname' = local (ptr thunk) thname
  tell [ status := getElementPtr' thname' [istr 0]
       , closureP := getElementPtr' thname' [istr 1]
       , Do $ store' (local (ptr i8) status) (constant $ intv8 constr)
       , val := bitcast' val' voidptr
       , Do $ store' (local (ptr voidptr) closureP) (local voidptr val)
       ]

genThunk :: String -> TExpr -> BlockGenerator ()
genThunk tname expr = do
  let thname = Name tname
      vars' = S.map qname $ ftv expr
  cls <- genClosure tname vars' $ \name' _ -> do
    startId <- genTemp
    genExpr startId name' expr
  updateThunk thname 0 $ local (ptr closure) cls

createThunk :: String -> TExpr -> BlockGenerator ()
createThunk tname expr = do
  let thname = Name tname
  storage <- alloc' thunk
  tell [ thname := assign (ptr thunk) storage ]
  genThunk tname expr

buildConstrs :: TTypes -> Constrs
buildConstrs = M.fromList . concatMap (\(Ann _ (TyDecl _ _ cs)) -> zipWith build [1..] cs) . I.toList
  where build i (Ann _ (TyCon (KName _ n) vs)) = (n, Constr { conId = i, conArgs = fromIntegral (length vs) })

generateLLVM :: Typechecked -> Module
generateLLVM prog = defaultModule { moduleName = "program"
                                  , moduleDefinitions = defs
                                  }

  where defs = execWriter $ flip runReaderT (buildConstrs $ progTypes prog) $ do
          builtinDefinitions

          runUniqueT $ do
            let resultArg = Name "result"
                toplevel = "toplevel"
                thname = Name toplevel
            forceId <- genTemp
            (resTest, forceCmds) <- runWriterT $ do
              createThunk toplevel (progExpr prog)
              force' thname
            goodId <- genTemp
            goodCmds <- execWriterT $ do
              intV <- getThunkVal int thname
              intR <- genTemp
              tell [ intR := load' (local (ptr int) intV)
                   , Do $ store' (local (ptr int) resultArg) (local int intR)
                   ]
            let goodTerm = Do $ Ret (Just $ constant $ intv 0) []
            failId <- genTemp
            let failTerm = Do $ Ret (Just $ constant $ intv 1) []
                forceTerm = Do $ CondBr (local i1 resTest) failId goodId []
                forceBlock = BasicBlock forceId forceCmds forceTerm
                goodBlock = BasicBlock goodId goodCmds goodTerm
                failBlock = BasicBlock failId [] failTerm

            let mdef = functionDefaults { returnType = int
                                        , name = Name "main"
                                        , parameters = ([Parameter (ptr int) resultArg []], False)
                                        , basicBlocks = [forceBlock, goodBlock, failBlock]
                                        }
            tellDef mdef
