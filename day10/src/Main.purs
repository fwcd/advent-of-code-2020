module Main where

import Prelude

import Data.Array (catMaybes, deleteAt, drop, elem, filter, group, head, length, modifyAt, nub, sort, tail, take, (!!), (..), (:))
import Data.Array.NonEmpty (toArray)
import Data.Foldable (maximum, product, sum)
import Data.Int.Parse (parseInt, toRadix)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

joltagePath :: Int -> Int -> Array Int -> Maybe (Array Int)
joltagePath j maxJ as | j == maxJ - 3 = Just [3]
                      | j >  maxJ - 3 = Nothing
                      | otherwise     = do
                        a <- head $ filter (\a -> a - j <= 3) $ take 3 as
                        as' <- tail as
                        case joltagePath a maxJ as' of
                          Just ds -> Just $ (a - j) : ds
                          Nothing -> joltagePath j maxJ as'

maybeToArray :: forall a. Maybe a -> Array a
maybeToArray (Just x) = [x]
maybeToArray Nothing  = []

countArrangements :: Array Int -> Int
countArrangements [x] | x <= 3 = 1
countArrangements xs = sum $ do
  i <- 0 .. (length xs - 1)
  j <- (i + 1) .. (length xs - 1)
  x <- maybeToArray $ xs !! i
  y <- maybeToArray $ xs !! j

  if x + y == 3 then 
    [1]
  else if x + y > 3 then
    []
  else do
    xs'  <- maybeToArray $ deleteAt j xs
    xs'' <- maybeToArray $ modifyAt i (add j) xs'
    pure $ 1 + countArrangements xs''

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "resources/input.txt"
  let joltages = sort $ catMaybes $ map (flip parseInt $ toRadix 10) $ split (Pattern "\n") input
      parts = do
        m <- maximum joltages
        path <- joltagePath 0 (m + 3) joltages
        let part1 = length (filter (eq 1) path) * length (filter (eq 3) path)
            chunksOf1s = map toArray $ filter (elem 1) $ group path
            part2 = product $ map countArrangements chunksOf1s
        pure $ Tuple part1 part2

  case parts of
    Just (Tuple part1 part2) -> do
      log $ "Part 1: " <> show part1
      log $ "Part 2: " <> show part2
    Nothing -> do
      log "No joltages!"
