{-|
  Module      : Reflex.Dom.RefleCss.Util
  Description : General Utilities
-}

module Reflex.Dom.RefleCSS.Util where

-- Base --
-- Third Party --
import qualified Data.Map as Map
import Data.Text hiding ( foldr )
import Reflex.Dom
-- Local --
-- End Imports --

cssFile :: MonadWidget t m => Text -> m (Event t ())
cssFile r = do
  (e, _) <- elAttr' "link" (Map.fromList [
      ("rel", "stylesheet")
    , ("type", "text/css")
    , ("href", r)]) $ return ()
  return $ domEvent Load e

meta :: MonadWidget t m => m ()
meta = elAttr "meta" (Map.fromList [
      ("name", "viewport")
    , ("content", "width=device-width,initial-scale=1")
    , ("charset", "UTF-8")
  ]) $ return ()

jsFile :: MonadWidget t m => Text -> m (Event t ())
jsFile file = do
  (e, _) <- elAttr' "script" (Map.fromList
    [ ("language", "javascript")
    , ("src", file)]) $ return ()
  return $ domEvent Load e

-- Anything in the existing attribute map will be overridden by vaules in the list.
inject :: AttributeMap -> [(Text, Text)] -> AttributeMap
inject = foldr (\(k, v) a -> Map.insert k v a)
