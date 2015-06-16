{
module Haskell.Parser
       ( HsTopsP
       , HsTop(..)
       , _TELet, _TEData
       , HsTopLet(..)
       , HsExpr(..)
       , HsLet(..)
       , HsPat(..)
       , parseHaskell
       ) where

import Control.Lens
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.ByteString.Lazy as BL

import Core.Types
import Core.Rename
import Haskell.Lexer

}

%name parseHaskellInt
%tokentype { Token }
%error { parseError }
%monad { Parser }
%lexer { nextToken } { EOF }

%token
  let    { Posed _ TLet }
  in     { Posed _ TIn }
  case   { Posed _ TCase }
  of     { Posed _ TOf }
  data   { Posed _ TData }
  '->'   { Posed _ TArrow }
  '::'   { Posed _ TOfType }
  '|'    { Posed _ TBar }
  int    { Posed _ (TInt _) }
  '='    { Posed _ TEq }
  '_'    { Posed _ TUnderscore }
  val    { Posed _ (TVal _) }
  con    { Posed _ (TCon _) }
  ';'    { Posed _ TSemicolon }
  ':'    { Posed _ TColon }
  op     { Posed _ (TOp _) }
  tyop   { Posed _ (TTyOp _) }
  '('    { Posed _ TOParen }
  ')'    { Posed _ TCParen }
  '{'    { Posed _ TOBrace }
  '}'    { Posed _ TCBrace }
  '"'    { Posed _ TQuote }
  strp   { Posed _ (TStringPart $$) }

%nonassoc '::'
%right in

%right '->'

%%

Begin :: { HsTopsP }
Begin : '{' Tops '}'               { $2 }

Tops :: { HsTopsP }
Tops : Top ';' Tops                { $1 : $3 }
     | {- empty -}                 { [] }

Top :: { HsTop Pos }
Top : TopLet                       { TELet $1 }
    | TyDecl                       { TEData $1 }

TopLet :: { HsTopLet Pos }
TopLet : val '=' Expr              { annPos $1 $ TLBind (extract $1) [] $3 }
       | val '::' Type              { annPos $1 $ TLAnn (extract $1) $3 }
       | val TopPats '=' Expr      { annPos $1 $ TLBind (extract $1) $2 $4 }

TyDecl :: { HsTyDecl Pos }
TyDecl : data con TyVarNames '=' TyCons { annPos $1 $ TyDecl (extract $2) $3 $5 } 

TyVarNames :: { [TyVarName] }
TyVarNames : val TyVarNames        { extract $1 : $2 }
           | {- empty -}           { [] }

TyCons :: { [HsTyCon Pos] }
TyCons : TyCon '|' TyCons          { $1 : $3 }
       | TyCon                     { [$1] }

TyCon :: { HsTyCon Pos }
TyCon : con TopTypes               { annPos $1 $ TyCon (extract $1) $2 }

TopTypes :: { [HsType Pos] }
TopTypes : TypeTerm TopTypes       { $1 : $2 }
      | {- empty -}                { [] }

Type :: { HsType Pos }
Type : Type '->' Type               { replace $1 $ TFun $1 $3 }
     | TyName TypeApp              { replace $1 $ TApp $1 $2 }

TyName :: { HsTyName Pos }
TyName : val                       { annPos $1 $ TNVar $ extract $1 }
       | con                       { annPos $1 $ TNLit $ extract $1 }

TypeApp :: { [HsType Pos] }
TypeApp : TypeTerm TypeApp         { $1 : $2 }
        | {- empty -}              { [] }

TypeTerm :: { HsType Pos }
TypeTerm : TyName                  { replace $1 $ TApp $1 [] }
         | '(' Type ')'            { $2 }

Let :: { HsLet Pos }
Let : val '::' Type                 { annPos $1 $ LAnn (extract $1) $3 }
    | val TopPats '=' Expr         { annPos $1 $ LFun (extract $1) $2 $4 }
    | Pat '=' Expr                 { replace $1 $ LPat $1 $3 }

TopPats :: { [HsPat Pos] }
TopPats : TopPat TopPats           { $1 : $2 }
        | TopPat                   { [$1] }

TopPat :: { HsPat Pos }
TopPat : OnePat                    { $1 }
       | con                       { annPos $1 $ HPCon (extract $1) [] }

Pat :: { HsPat Pos }
Pat : OnePat                       { $1 }
    | con TopPats                  { annPos $1 $ HPCon (extract $1) $2 }

OnePat :: { HsPat Pos }
OnePat : val                       { annPos $1 $ HPVar (extract $1) }
       | '_'                       { annPos $1 HPWildCard }
       | '(' Pat ')'               { $2 }

Expr :: { HsExpr Pos }
  : Expr '::' Type                       { replace $1 $ HTypeAnn $3 $1 }
  | BeginLet let '{' Lets '}' in Expr   { annPos $2 $ HLet $4 $7 }
  | case Expr BeginCase of '{' Alts '}' { annPos $1 $ HCase $2 $6 }
  | ExprApp                             { $1 }

BeginLet :: { () }
BeginLet : {- empty -}             {% startLayout (Just TIn) }

BeginCase :: { () }
BeginCase : {- empty -}            {% startLayout Nothing }

Lets :: { [HsLet Pos] }
Lets : Let ';' Lets                { $1 : $3 }
     | {- empty -}                 { [] }

Alts :: { HsAlts meta }
Alts : Pat '->' Expr ';' Alts       { ($1, $3) : $5 }
         | {- empty -}             { [] }

ExprApp :: { HsExpr Pos }
  : ExprApp ExprTerm               { replace $1 $ HApply $1 $2 }
  | ExprTerm                       { $1 }

ExprTerm :: { HsExpr Pos }
ExprTerm : val                     { annPos $1 $ HVar $ extract $1 }
         | con                     { annPos $1 $ HLit $ extract $1 }
         | '(' Expr ')'            { $2 }

String :: { Text }
String : '"' StringP '"'           { TL.toStrict $ TB.toLazyText $2 }

StringP :: { Builder }
StringP
  : strp StringP                   { $1 <> $2 }
  | {- empty -}                    { mempty }

{

extract :: FromToken t => Token -> t
extract (Posed _ p) = extract' p

-- helpful class to reduce verbosity in the parser
class FromToken t where
  extract' :: Token' -> t

instance FromToken TyVarName where
  extract' (TVal t) = TyVarName t

instance FromToken TyLitName where
  extract' (TCon t) = TyLitName t

instance FromToken ValLitName where
  extract' (TCon t) = ValLitName t

instance FromToken ValVarName where
  extract' (TVal t) = ValVarName t

type HsTyName meta = TyName meta TyVarName TyLitName

type HsType meta = Type meta TyVarName TyLitName

type HsTyCon meta = TyCon meta TyVarName TyLitName

type HsTyDecl meta = TyDecl meta TyVarName TyLitName

data HsPat' meta = HPVar ValVarName
                 | HPWildCard
                 | HPCon ValLitName [HsPat meta]
                 deriving (Show, Eq)

type HsPat meta = Ann meta HsPat'

data HsLet' meta = LAnn ValVarName (HsType meta)
                 | LFun ValVarName [HsPat meta] (HsExpr meta)
                 | TLBind ValVarName [HsPat meta] (HsExpr meta)
                 | LPat (HsPat meta) (HsExpr meta)
                 deriving (Show, Eq)

type HsLet meta = Ann meta HsLet'

type HsAlts meta = [(HsPat meta, HsExpr meta)]

data HsExpr' meta = HLet [HsLet meta] (HsExpr meta)
                  | HCase (HsExpr meta) (HsAlts meta)
                  | HVar ValVarName
                  | HLit ValLitName
                  | HApply (HsExpr meta) (HsExpr meta)
                  | HTypeAnn (HsType meta) (HsExpr meta)
                  deriving (Show, Eq)

type HsExpr meta = Ann meta HsExpr'

data HsTopLet' meta = TLAnn ValVarName (HsType meta)
                    | TLBind ValVarName [HsPat meta] (HsExpr meta)
                    deriving (Show, Eq)

type HsTopLet meta = Ann meta HsTopLet'

data HsTop meta = TELet (HsTopLet meta)
                | TEData (HsTyDecl meta)
                deriving (Show, Eq)

makePrisms ''HsTop

type HsTopsP = [HsTop Pos]

annPos :: Token -> a -> Ann' Pos a
annPos t = Ann $ tpos t

replace :: Functor f => f a -> b -> f b
replace = flip (<$)

parseError :: Token -> Parser a
parseError t = do
    Pos l c <- pos
    parserFail $ "parse error at line " ++ show l ++ ", column " ++ show c
      ++ ": unexpected token " ++ show t

lexer :: (Token -> Parser a) -> Parser a
lexer = nextToken

parseHaskell :: BL.ByteString -> Either String HsTopsP
parseHaskell s = runParser s $ do
  startLayout Nothing
  parseHaskellInt

}
