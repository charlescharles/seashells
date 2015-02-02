module Main where

import           Data.List      (lookup)
import           Data.Maybe     (fromJust)
import           Graphics.Gloss

data Cell = O | X deriving (Eq)

instance Show Cell where
    show X = "O"
    show O = " "

type World = [Cell]

type Rule = Cell -> Cell -> Cell -> Cell

fromList :: [((Cell, Cell, Cell), Cell)] -> Cell -> Cell -> Cell -> Cell
fromList ls a b c = fromJust $ lookup (a, b, c) ls

rule90 = fromList [ ((X, X, X), O)
                  , ((X, X, O), X)
                  , ((X, O, X), O)
                  , ((X, O, O), X)
                  , ((O, X, X), X)
                  , ((O, X, O), O)
                  , ((O, O, X), X)
                  , ((O, O, O), O) ]

rule30 = fromList [ ((X, X, X), O)
                  , ((X, X, O), O)
                  , ((X, O, X), O)
                  , ((X, O, O), X)
                  , ((O, X, X), X)
                  , ((O, X, O), X)
                  , ((O, O, X), X)
                  , ((O, O, O), O) ]

once :: Rule -> World -> World
once f w = once' f ([last w] ++ w ++ [head w])

once' :: Rule -> World -> World
once' f (a:b:c:xs) = f a b c : once' f (b:c:xs)
once' _ _ = []

alone :: Int -> World
alone n = e ++ [X] ++ e where
    e = replicate (n `div` 2) O

evolve :: Rule -> World -> Int -> [World]
evolve f w n = take n $ iterate (once f) w

showWorld :: World -> String
showWorld = concatMap show

displayWorld :: [World] -> IO ()
displayWorld = mapM_ (putStrLn . showWorld)

test :: IO ()
test = displayWorld testHist

testHist = evolve rule30 (alone 170) 500

main = display (InWindow "Nice Window" (200, 200) (10, 10)) white pic

sqr = rectangleSolid 20 20

pic = Pictures [Translate (-50) (-50) sqr, circle 80]
