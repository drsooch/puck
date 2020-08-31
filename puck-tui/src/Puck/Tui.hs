module Puck.Tui (puckMain) where

import           Brick


test1 :: Bool -> Int -> Int
test1 b n = if b then n else n

ui :: Widget ()
ui = str "Hello, World!"

puckMain :: IO ()
puckMain = simpleMain ui
