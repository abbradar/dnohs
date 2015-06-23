{
module Haskell.Lexer
       ( Token'(..)
       , Token(..)
       , Parser
       , parserFail
       , tpos
       , pos
       , startLayout
       , nextToken
       , runParser
       ) where

import Control.Monad
import Control.Lens
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TB
import Control.Monad.State

import Core.Types (Pos(..))

}

%wrapper "monadUserState-bytestring"

$digit = [0-9]
$valueFst = [_a-z] -- first letter of a value
$typeFst = [A-Z] -- first letter of a type or a constructor
$letter = [[$valueFst$typeFst]0-9']
$symbol = [\+\-\!\@\#\$\%\^\&\*\.]

tokens :-
            $white+           ;
  <0>       "--".*            ;
  <0>       "{-"              { openComment `andBegin` comment }
  <comment> [^\-\{]+          ;
  <comment> "{-"              { openComment }
  <comment> "-}"              { closeComment }
  <comment> .                 ;
  <0>       let               { val TLet }
  <0>       in                { val TIn }
  <0>       case              { val TCase }
  <0>       of                { val TOf }
  <0>       data              { val TData }
  <0>       "->"              { val TArrow }
  <0>       "::"              { val TOfType }
  <0>       "|"               { val TBar }
  <0>       $digit+           { tok $ TInt . toInt }
  <0>       "="               { val TEq }
  <0>       "_"               { val TUnderscore }
  <0>       $valueFst$letter* { tok $ TVal . toText }
  <0>       $typeFst$letter*  { tok $ TCon . toText }
  <0>       ";"               { val TSemicolon }
  <0>       ":"[$symbol\:]*   { tok $ TTyOp . toText }
  <0>       $symbol+          { tok $ TOp . toText }
  <0>       "("               { val TOParen }
  <0>       ")"               { val TCParen }
  <0>       "{"               { val TOBrace }
  <0>       "}"               { val TCBrace }
  <0>       "\""              { val TQuote `andBegin` string }
  <string>  [^\"]+            { tok $ TStringPart . toPart }
  <string>  "\\".             { tok $ TStringPart . toPart . BL.tail }
  <string>  "\""              { val TQuote `andBegin` 0 }
{

type Parser = Alex

data Token' = TLet
            | TIn
            | TCase
            | TOf
            | TData
            | TArrow
            | TOfType
            | TBar
            | TInt !Integer
            | TEq
            | TUnderscore
            | TVal !Text
            | TCon !Text
            | TSemicolon
            | TColon
            | TTyOp !Text
            | TOp !Text
            | TOParen
            | TCParen
            | TOBrace
            | TCBrace
            | TQuote
            | TStringPart !Builder
            deriving (Show, Eq)

data Layout = Lined Int (Maybe Token')
            | Braced

data AlexUserState = ParserState { _layoutStack :: [Layout]
                                 , _awaitLine :: Maybe (Maybe Token')
                                 , _awaitingToken :: Maybe (Alex Token)
                                 , _commentLevel :: Int
                                 }

-- We can't use TH here, sadly
layoutStack :: Lens' AlexUserState [Layout]
layoutStack = lens _layoutStack (\s v -> s { _layoutStack = v })

awaitLine :: Lens' AlexUserState (Maybe (Maybe Token'))
awaitLine = lens _awaitLine (\s v -> s { _awaitLine = v })

awaitingToken :: Lens' AlexUserState (Maybe (Alex Token))
awaitingToken = lens _awaitingToken (\s v -> s { _awaitingToken = v })

commentLevel :: Lens' AlexUserState Int
commentLevel = lens _commentLevel (\s v -> s { _commentLevel = v })

instance MonadState AlexUserState Alex where
  get = Alex $ \s -> Right (s, alex_ust s)
  put us = Alex $ \s -> Right (s { alex_ust = us }, ())

data Token = Posed !(Maybe Pos) !Token'
           | EOF
           deriving (Show, Eq)

tval :: Token -> Token'
tval (Posed _ val) = val

alexEOF :: Alex Token
alexEOF = do
  s <- get
  case s^.layoutStack of
   (Lined _ _):_ -> do
     layoutStack %= tail
     awaitingToken .= Just (return $ Posed Nothing TCBrace)
     return $ Posed Nothing TSemicolon
   _ -> return EOF

convertPos :: AlexPosn -> Pos
convertPos (AlexPn _ l c) = Pos l c

alexInitUserState :: AlexUserState
alexInitUserState = ParserState { _layoutStack = []
                                , _awaitLine = Just Nothing
                                , _awaitingToken = Nothing
                                , _commentLevel = 0
                                }

toText :: ByteString -> Text
toText = TL.toStrict . TL.decodeUtf8

toInt :: ByteString -> Integer
toInt bs = i
  where Just (i, "") = BL.readInteger bs

toPart :: ByteString -> Builder
toPart = TB.fromLazyText . TL.decodeUtf8

tok :: (ByteString -> Token') -> AlexAction Token
tok f (convertPos -> pos@(Pos _ c), _, str, _) len = prevLayout
  where tok' = f $ BL.take len str
        tok = Posed (Just pos) tok'
         
        prevLayout = do
          s <- get
          case s^.layoutStack of
            (Lined c' end):_
              | c < c' || maybe False (== tok') end -> do
                layoutStack %= tail
                let next = do
                      awaitingToken .= Just prevLayout
                      return $ Posed Nothing TCBrace
                awaitingToken .= Just next
                return $ Posed Nothing TSemicolon
              | c == c' -> do
                awaitingToken .= Just currLayout
                return $ Posed Nothing TSemicolon
            (Braced:_)
              | tok' == TCBrace -> do
                  layoutStack %= tail
                  currLayout
            _ -> currLayout

        currLayout = do
          s <- get
          case s^.awaitLine of
           Just end -> do
             awaitLine .= Nothing
             let next = return tok
             if maybe False (== tok') end
               then do
                 let next1 = do
                       awaitingToken .= Just next
                       return $ Posed Nothing TCBrace
                 awaitingToken .= Just next1
               else if tok' == TOBrace
                    then layoutStack %= (Braced:)
                    else do
                      layoutStack %= (Lined c end :)
                      awaitingToken .= Just next
             return $ Posed Nothing TOBrace
           Nothing -> return tok

openComment :: AlexAction Token
openComment _ _ = do
  commentLevel += 1
  alexMonadScan

closeComment :: AlexAction Token
closeComment _ _ = do
  commentLevel -= 1
  s <- get
  when (s^.commentLevel == 0) $ alexSetStartCode 0
  alexMonadScan

tpos :: Token -> Pos
tpos (Posed (Just p) _) = p

val :: Token' -> AlexAction Token
val = tok . const

pos :: Parser Pos
pos = Alex $ \s -> Right (s, convertPos $ alex_pos s)

startLayout :: Maybe Token' -> Parser ()
startLayout end = awaitLine .= Just end
  
nextToken :: (Token -> Parser a) -> Parser a
nextToken next = do
  s <- get
  t <- case s^.awaitingToken of
    Just run -> do
      awaitingToken .= Nothing
      run
    Nothing -> alexMonadScan
  next t

parserFail :: String -> Parser a
parserFail = alexError

runParser :: ByteString -> Parser a -> Either String a
runParser = runAlex

}
