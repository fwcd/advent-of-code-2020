module Day01 where

part1 :: [Int] -> Int
part1 nums = head $ [x * y | x <- nums, y <- nums, x + y == 2020]

part2 :: [Int] -> Int
part2 nums = head $ [x * y * z | x <- nums, y <- nums, z <- nums, x + y + z == 2020]

main :: IO ()
main = do
    raw <- readFile "inputs/Day01.txt"
    let nums = read <$> lines raw
    putStrLn $ "Part 1: " ++ show (part1 nums)
    putStrLn $ "Part 2: " ++ show (part2 nums)
