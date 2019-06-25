module Main where

import Lens.Micro.Platform ( (^.) )
import Data.Text
import Language.Javascript.JSaddle
import Reflex.Dom

import qualified Frontend.Bootstrap as B
import qualified Frontend.Materialize as M

main :: IO ()
main = mainWidgetWithHead header body

header :: MonadWidget t m => m ()
header = do
  c <- liftJSM check
  if c
  then M.header
  else B.header

body :: MonadWidget t m => m ()
body = do
  c <- liftJSM check
  if c
  then M.body
  else B.body

check :: JSM Bool
check = do
  win <- jsg ("window"::Text)
  loc <- win ^. js ("location"::Text)
  href <- valToText =<< loc ^. js ("search"::Text)
  return (isInfixOf "materialize"  href)
