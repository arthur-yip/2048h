module Game where

import Data.List
import System.Random

boardSize :: Int
boardSize = 16

empty :: [Int]
empty = replicate boardSize 0

initGame :: IO [Int]
initGame = do
  g <- place empty
  g' <- place g
  return g'

win :: [Int] -> Bool
win = elem 2048

lose :: [Int] -> Bool
lose game = let games' = [ shiftUp game, shiftLeft game, shiftDown game, shiftRight game ] in
            all (== game) games'

place :: [Int] -> IO [Int]
place game = do
  g <- newStdGen
  let (location, _) = randomR (0, (length game) - 1) g
  if game !! location == 0 then (replace game location) else place game

replace :: [Int] -> Int -> IO [Int]
replace game loc = do
  v <- newCell
  return $ replaceAtIndex loc v game

newCell :: IO Int
newCell = do
  g <- newStdGen
  let (v, _) = randomR (1, 10) g :: (Int, StdGen)
  return $ if v == 1 then 4 else 2

shiftUp :: [Int] -> [Int]
shiftUp = shiftGame transpose transpose

shiftLeft :: [Int] -> [Int]
shiftLeft = shiftGame id id

shiftDown :: [Int] -> [Int]
shiftDown = shiftGame ((map reverse) . transpose) (transpose . (map reverse))

shiftRight :: [Int] -> [Int]
shiftRight = shiftGame (map reverse) (map reverse)

shiftGame :: ([[Int]] -> [[Int]]) -> ([[Int]] -> [[Int]]) -> [Int] -> [Int]
shiftGame f g = concat . g . shiftArrays . f . (splitAtEvery 4)

shiftArrays :: [[Int]] -> [[Int]]
shiftArrays xs = map (fillZeros . combine . (filter (\x -> x /= 0))) xs

combine :: [Int] -> [Int]
combine (x0:x1:xs)
  | x0 == x1 = (x0 * 2):(combine xs)
  | otherwise = x0:(combine (x1:xs))
combine xs = xs

fillZeros :: [Int] -> [Int]
fillZeros xs = take 4 $ xs ++ (repeat 0)

splitAtEvery :: Int -> [a] -> [[a]]
splitAtEvery n xs | (length xs <= n) = [xs]
splitAtEvery n xs = let (x, rest) = splitAt n xs in
                    [x] ++ (splitAtEvery n rest)

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls
