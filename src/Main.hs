module Main where

import           Control.Monad      (sequence)
import           Data.List          (lookup)
import           Data.Maybe         (fromJust)
import           Graphics.Gloss
import           System.Environment (getArgs)

data Cell = Dead | Alive deriving (Eq, Show)

type World = [Cell]

type Rule = Cell -> Cell -> Cell -> Cell

fromList :: [Cell] -> Rule
fromList s a b c = fromJust $ lookup (a, b, c) r where
  r = zipWith f (sequence $ replicate 3 [Alive, Dead]) s
  f [x, y, z] st = ((x, y, z), st)

fromString :: String -> Rule
fromString = fromList . map toState where
  toState c = case c of
    '1' -> Alive
    '0' -> Dead

rule30 = fromString "00011110"
rule90 = fromString "01011010"

once :: Rule -> World -> World
once f w = once' f ([last w] ++ w ++ [head w])

once' :: Rule -> World -> World
once' f (a:b:c:xs) = f a b c : once' f (b:c:xs)
once' _ _ = []

alone :: Int -> World
alone n = e ++ [Alive] ++ e where
    e = replicate (n `div` 2) Dead

evolve :: Rule -> World -> Int -> [World]
evolve f w n = take n $ iterate (once f) w

evolveSingle :: Rule -> Int -> [World]
evolveSingle f n = evolve f (alone n) n

testHist = evolve rule30 (alone 500) 500

size = (700, 700)

loc = (50, 50)

main :: IO ()
main = do
  [r, n] <- getArgs
  let steps = read n :: Int
  let rule = case r of
        "rule30" -> rule30
        "rule90" -> rule90
  displayEvo rule steps

displayEvo :: Rule -> Int -> IO ()
displayEvo r n = display' pic where
  display' = display (InWindow "Seashells" size loc) white
  pic = displayScaledHist (evolveSingle r n) size

displayScaledHist :: [World] -> (Int, Int) -> Picture
displayScaledHist h (x, y) = Scale s s (displayHist h) where
  s = max (fromIntegral x / width) (fromIntegral y / height)
  width = (fromIntegral . length . head) h
  height = (fromIntegral . length) h

pic = displayScaledHist testHist size

displayCell :: Cell -> Picture
displayCell Dead = Blank
displayCell Alive = rectangleSolid 1 1

displayWorld :: World -> Picture
displayWorld w = smear (map displayCell w) XAxis 1

displayHist :: [World] -> Picture
displayHist h = smear (reverse $ map displayWorld h) YAxis 1

data Axis = XAxis | YAxis deriving (Eq, Show)

smear :: [Picture] -> Axis -> Float -> Picture
smear ps ax d = Pictures $ zipWith trans ps offsets where
  trans p e = case ax of
    XAxis -> Translate e 0 p
    YAxis -> Translate 0 e p
  offsets = iterate (+ d) ((-d) * (n - 1) / 2)
  n = fromIntegral (length ps)
