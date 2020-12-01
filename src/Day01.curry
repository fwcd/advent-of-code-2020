module Day01 where

multCorrect :: Int -> Int -> Int
multCorrect x y | x + y == 2020 = x * y

solution :: Int
solution = multCorrect (anyOf nums) (anyOf nums)
    where nums = [1721, 979, 366, 299, 675, 1456]
