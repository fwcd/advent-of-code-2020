module Day01 where

part1 :: [Int] -> Int
part1 nums = head $ [x * y | x <- nums, y <- nums, x + y == 2020]

main :: IO ()
main = do
    raw <- readFile "inputs/Day01.txt"
    let nums = read <$> lines raw
    putStrLn $ "The solution for part 1 is " ++ show (part1 nums)
