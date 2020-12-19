module Main where

import Control.Applicative ((<*), (*>))
import Text.Parsec
import Text.Parsec.String

data Symbol = Terminal String | Nonterminal Int deriving Show
type Arm = [Symbol]
data Rule = Rule Int [Arm] deriving Show
type CFG = [Rule]
data Input = Input CFG [String] deriving Show

main :: IO ()
main = do
    result <- parseFromFile input "resources/example.txt"
    case result of
        Right i -> putStrLn $ show i
        Left e -> error $ "Parse error: " ++ show e

input :: Parser Input
input = do
    g <- cfg
    _ <- newline
    es <- examples
    return $ Input g es

cfg :: Parser CFG
cfg = many1 (rule <* newline)

rule :: Parser Rule
rule = do
    i <- int
    _ <- symbol $ char ':'
    arms <- arm `sepBy1` (symbol $ char '|')
    return $ Rule i arms

arm :: Parser Arm
arm = many1 (symbol symb)

symb :: Parser Symbol
symb = terminal <|> nonterminal

terminal :: Parser Symbol
terminal = Terminal <$> quotedString

nonterminal :: Parser Symbol
nonterminal = Nonterminal <$> int

examples :: Parser [String]
examples = many1 (many1 (noneOf "\n") <* newline)

int :: Parser Int
int = read <$> many1 digit

quotedString :: Parser String
quotedString = char '"' *> many (noneOf ['"']) <* char '"'

symbol :: Parser a -> Parser a
symbol p = p <* many (oneOf " ")
