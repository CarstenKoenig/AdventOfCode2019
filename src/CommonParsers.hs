module CommonParsers 
  ( Parser
  , nameP
  , numberP
  , boolP
  , brace
  , chainL1
  ) where

import Control.Applicative ((<|>))
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC

type Parser = Parsec Void String

numberP :: (Read a, Num a) => Parser a
numberP = P.label "number" $ P.hidden $
  read <$> P.some (PC.char '-' <|> PC.numberChar)


nameP :: Parser String
nameP = P.label "name" $ P.hidden $
  P.some PC.alphaNumChar

boolP :: Parser Bool
boolP = P.label "boolean" $ P.hidden $ P.choice
    [ True <$ PC.string "true"
    , False <$ PC.string "false" 
    ]


brace :: Parser p -> Parser p
brace p = do
  _ <- PC.char '(' <* P.hidden PC.space
  res <- p
  _ <- PC.char ')' <* P.hidden PC.space
  pure res


chainL1 :: Parser (a -> a -> a) -> Parser a -> Parser a
chainL1 pOp pVal = do
  first <- pVal 
  more first
  where
    more acc =
      do
        op <- pOp
        val <- pVal
        more (op acc val)
      <|> pure acc
