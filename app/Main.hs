module Main where

import MyLib
import Numeric (showFFloat)
import Data.List (intercalate)
import Control.Monad (replicateM)

showP :: Point  -> String
showP (x, y) = showString "(" . showf x . showString "," . showf y $ ")"
  where showf = showFFloat (Just 3)

showLP :: [Point] -> String
showLP lp = showString ('[' : intercalate "," (showP <$> lp)) "]"

-- >>> fmap showLP <$> replicateM 100 $ (naivePointInCircle 2)
-- >>> fmap showLP <$> replicateM 100 $ (correctPointInCircle 2)

main :: IO ()
main = do
  putStrLn . showLP =<< replicateM 1000 (naivePointInCircle 2)
  putStrLn . showLP =<< replicateM 1000 (correctPointInCircle 2)
  putStrLn "Hello, Haskell!"
