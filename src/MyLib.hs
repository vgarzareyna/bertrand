{-# LANGUAGE TypeApplications #-}
module MyLib (naivePointInCircle, correctPointInCircle, dist, Point)  where

import System.Random
import Numeric (showFFloat)
import Data.List (intercalate)

type Point = (Double, Double)

naivePointInCircle :: Double -> IO Point
naivePointInCircle r = do 
  angle  <- randomRIO (0.0, 2.0*pi)
  radius <- randomRIO (0.0, r)
  let x = radius * cos angle 
  let y = radius * sin angle
  pure (x, y)

correctPointInCircle :: Double -> IO Point
correctPointInCircle r = do 
  angle  <- randomRIO (0.0, 2.0*pi)
  radius <- sqrt <$> randomRIO (0.0, r ** 2)
  let x = radius * cos angle 
  let y = radius * sin angle
  pure (x, y)

dist (x,y) = sqrt $ x**2 + y**2

