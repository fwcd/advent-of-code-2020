module Main where

import Control.Applicative ( (<*), (*>) )
import Control.Monad ( void )
import Data.Either ( isRight )
import Data.Functor ( ($>) )
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

data Part = Part1 | Part2

main :: IO ()
main = do
    result <- parseFromFile input "resources/example2.txt"
    case result of
        Right (Input g es) -> do
            putStrLn $ "Success fully parsed " ++ show (length g) ++ " rules and " ++ show (length es) ++ " examples!"

            let p1 = cfgParser Part1 g
                p2 = cfgParser Part2 g
                part1 = length $ filter (isRight . parse p1 "input examples") es
                part2 = length $ filter (isRight . parse p2 "input examples") es

            putStrLn $ "Part 1: " ++ show part1
            putStrLn $ "Part 2: " ++ show part2
        Left e -> error $ "Parse error: " ++ show e

-- Dynamic parser generation from CFG

cfgParser :: Part -> CFG -> Parser ()
cfgParser p g = ruleParser p g (lookupRule 0 g) *> eof

-- Note how we cannot use the standard many/optional combinators, since we do not want greedy matching
-- We therefore refactor the rules:
--
--     0: 8 11
--     8: 42 | 42 8
--     11: 42 31 | 42 11 31
--
-- into
--
--     0: 8 (31 | 11)
--     8: 42 42+    <-- note how the last 42 was previously matched by 11, but is now matched greedily
--     11: 42 11 31
--
ruleParser :: Part -> CFG -> Rule -> Parser ()
ruleParser Part2 g (Rule 0  as) = ruleParser Part2 g (lookupRule 8 g) <* (try (ruleParser Part2 g (lookupRule 11 g)) <|> ruleParser Part2 g (lookupRule 31 g))
ruleParser Part2 g (Rule 8  as) = ruleParser Part1 g (lookupRule 42 g) <* many1 (ruleParser Part1 g (lookupRule 42 g))
ruleParser Part2 g (Rule 11 as) = void $ between (ruleParser Part1 g (lookupRule 42 g))
                                                 (ruleParser Part1 g (lookupRule 31 g))
                                                 (optionMaybe (ruleParser Part2 g (Rule 11 as)))
ruleParser p     g (Rule _  as) = choice $ map (try . armParser p g) as

armParser :: Part -> CFG -> Arm -> Parser ()
armParser p g = sequence_ . map (symbParser p g)

symbParser :: Part -> CFG -> Symbol -> Parser ()
symbParser _ _ (Terminal t) = void $ string t
symbParser p g (Nonterminal i) = ruleParser p g $ lookupRule i g

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

-- Utils

int :: Parser Int
int = read <$> many1 digit

quotedString :: Parser String
quotedString = char '"' *> many (noneOf ['"']) <* char '"'

symbol :: Parser a -> Parser a
symbol p = p <* many (oneOf " ")
