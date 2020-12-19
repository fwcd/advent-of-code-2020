module Main where

import Control.Applicative ( (<*), (*>) )
import Control.Monad ( void )
import Text.Parsec
import Text.Parsec.String

data Symbol = Terminal String | Nonterminal Int deriving Show
type Arm = [Symbol]
data Rule = Rule Int [Arm] deriving Show
type CFG = [Rule]
data Input = Input CFG [String] deriving Show

lookupRule :: Int -> CFG -> Rule
lookupRule i [] = error $ "No rule for " ++ show i
lookupRule i (r@(Rule j _) : rs) | i == j    = r
                                 | otherwise = lookupRule i rs

main :: IO ()
main = do
    result <- parseFromFile input "resources/example.txt"
    case result of
        Right i -> putStrLn $ show i
        Left e -> error $ "Parse error: " ++ show e

-- Dynamic parser generation from CFG

cfgParser :: CFG -> Parser ()
cfgParser g = choice $ map (ruleParser g) g

ruleParser :: CFG -> Rule -> Parser ()
ruleParser g (Rule _ as) = choice $ map (armParser g) as

armParser :: CFG -> Arm -> Parser ()
armParser g = sequence_ . map (symbParser g)

symbParser :: CFG -> Symbol -> Parser ()
symbParser _ (Terminal t) = void $ string t
symbParser g (Nonterminal i) = ruleParser g $ lookupRule i g

-- Input grammar parser

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
