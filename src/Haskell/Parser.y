{
module Haskell.Parser
       ( parseHaskell
       ) where

import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.ByteString.Lazy as BL

import Core.Types
import Core.Rename
import Haskell.Lexer
import Haskell.Types

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
Top : Let                          { TELet $1 }
    | TyDecl                       { TEData $1 }

TyDecl :: { HsTyDecl Pos }
TyDecl : data con TyVars '=' TyCons { annPos $1 $ TyDecl (extractText $2) $3 $5 } 

TyVars :: { [Name] }
TyVars : val TyVars                { extractText $1 : $2 }
       | {- empty -}               { [] }

TyCons :: { [HsTyCon Pos] }
TyCons : TyCon '|' TyCons          { $1 : $3 }
       | TyCon                     { [$1] }

TyCon :: { HsTyCon Pos }
TyCon : con TopTypes               { annPos $1 $ TyCon (extractText $1) $2 }

TopTypes :: { [HsType Pos] }
TopTypes : TypeTerm TopTypes       { $1 : $2 }
      | {- empty -}                { [] }

Type :: { HsType Pos }
Type : Type '->' Type              { replace $1 $ TApp (replace $1 $ TApp (annPos $2 TFun) $1) $3 }
     | TypeApp                     { $1 }

TypeTerm :: { HsType Pos }
TypeTerm : val                     { annPos $1 $ TVar $ extractText $1 }
         | con                     { annPos $1 $ TLit $ extractText $1 }
         | '(' Type ')'            { $2 }

TypeApp :: { HsType Pos }
TypeApp : TypeApp TypeTerm         { replace $1 $ TApp $1 $2 }
        | TypeTerm                 { $1 }

Let :: { HsLet Pos }
Let : val '::' Type                 { annPos $1 $ LAnn (extractText $1) $3 }
    | val TopPats '=' Expr         { annPos $1 $ LBind (extractText $1) $2 $4 }

TopPats :: { [HsPat Pos] }
TopPats : TopPat TopPats           { $1 : $2 }
        | {- empty -}              { [] }

TopPat :: { HsPat Pos }
TopPat : OnePat                    { $1 }
       | con                       { annPos $1 $ HPCon (extractText $1) [] }

Pat :: { HsPat Pos }
Pat : OnePat                       { $1 }
    | con TopPats                  { annPos $1 $ HPCon (extractText $1) $2 }

OnePat :: { HsPat Pos }
OnePat : val                       { annPos $1 $ HPVar $ extractText $1 }
--       | int                       { annPos $1 $ HPInt $ extractInt $1 }
       | '_'                       { annPos $1 HPWildCard }
       | '(' Pat ')'               { $2 }

Expr :: { HsExpr Pos }
  : Expr '::' Type                      { replace $1 $ HTyAnn $3 $1 }
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
  : ExprApp ExprTerm               { replace $1 $ HApp $1 $2 }
  | ExprTerm                       { $1 }

ExprTerm :: { HsExpr Pos }
ExprTerm : val                     { annPos $1 $ HVar $ extractText $1 }
         | int                     { annPos $1 $ HInt $ extractInt $1 }
         | con                     { annPos $1 $ HLit $ extractText $1 }
         | '(' Expr ')'            { $2 }

String :: { Text }
String : '"' StringP '"'           { TL.toStrict $ TB.toLazyText $2 }

StringP :: { Builder }
StringP
  : strp StringP                   { $1 <> $2 }
  | {- empty -}                    { mempty }

{

extractText :: Token -> Text
extractText (Posed _ (TVal v)) = v
extractText (Posed _ (TCon t)) = t

extractInt :: Token -> Integer
extractInt (Posed _ (TInt i)) = i

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
