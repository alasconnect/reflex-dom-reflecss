{-# LANGUAGE
    LambdaCase
#-}
{-|
  Module      : Reflex.Dom.RefleCSS.Materialize.V1.Shim
  Description : Inject CSS to shim css classes for Materialize
-}

module Reflex.Dom.RefleCSS.Materialize.V1.Shim (cssFile') where

-- Base --
-- Third Party --
import qualified Data.Text as T
import Lens.Micro.Platform ( (^.) )
import Reflex.Dom
-- Local --
import Reflex.Dom.RefleCSS.Class
import Reflex.Dom.RefleCSS.Materialize.V1.Class
-- End Imports --

-- |Injects the css file's contents directly into a script tag.
-- This allows the shim to override the imported classes.
cssFile' :: MonadWidget t m => T.Text -> m (Event t ())
cssFile' r = do
  ev <- getPostBuild
  evReq <- performRequestAsync $ fmap (\x -> XhrRequest "GET" x def) (r <$ ev)
  dyEv <- widgetHold (return never) (fmap f evReq)
  return $ switchPromptlyDyn dyEv
  where
    f req = case req ^. xhrResponse_responseText of
      Just t -> el "style" (text t >> text excessiveUseOfForce) >> getPostBuild
      Nothing -> el "style" (text ("\\* Error Injecting: " <> r <> " *\\")) >> getPostBuild

excessiveUseOfForce :: T.Text
excessiveUseOfForce = T.unwords $
  d <>
  containers <>
  orders <>
  justify <>
  align <>
  alignItems <>
  alignSelf <>
  alignContent <>
  w <>
  h <>
  mw <>
  mh <>
  vw <>
  vh <>
  mvw <>
  mvh <>
  m <>
  p <>
  t <>
  flex <>
  float <>
  btn <>
  sr <>
  utl
  where
    ps = T.pack . show
    containers = [ ".container-fluid { width: 100%; }" ]
    orders =
      [ ".order { order: 0; }"
      , ".order-first { order: 0; }"
      , ".order-last { order: 999; }"
      , foldr (\b a -> a <> ".order-" <> ps b <> "{order: " <> ps b <> ";}") "" [0..12]
      ]
    justify =
      [ ".justify-content-start { justify-content: flex-start; }"
      , ".justify-content-end { justify-content: flex-end; }"
      , ".justify-content-center { justify-content: center; }"
      , ".justify-content-between { justify-content: space-between; }"
      , ".justify-content-around { justify-content: space-around; }"
      ]
    align =
      [ ".align-baseline { vertical-align: baselines; }"
      , ".align-top { vertical-align: top; }"
      , ".align-middle { vertical-align: middle; }"
      , ".align-bottom { vertical-align: bottom; }"
      , ".align-text-top { vertical-align: text-top; }"
      , ".align-text-bottom { vertical-align: sub; }"
      ]
    alignItems = alignments "align-items"
    alignSelf = alignments "align-self"
    alignContent = alignments "align-content"
    w = percentKeyProperty "w" "width"
    h = percentKeyProperty "h" "height"
    mw = percentKeyProperty "mw" "max-width"
    mh = percentKeyProperty "mh" "max-height"
    vw = percentKeyPropertySuffix "vw" "width" "vw"
    vh = percentKeyPropertySuffix "vh" "height" "vh"
    mvw = percentKeyPropertySuffix "min-vw" "min-width" "vw"
    mvh = percentKeyPropertySuffix "min-vh" "min-height" "vh"

    m' s = foldr (\g a -> a <> "." <> buildClass (M s NoSize g) <> styleFromGap "margin" s g) ""
      [Gap0, Gap1, Gap2, Gap3, Gap4, Gap5, GapNeg1, GapNeg2, GapNeg3, GapNeg4, GapNeg5, GapAuto]
    m = foldr (\b a -> a <> [m' b]) [] [TopSide, BottomSide, LeftSide, RightSide, XSides, YSides, AllSides]
    p' s = foldr (\g a -> a <> "." <> buildClass (P s NoSize g) <> styleFromGap "padding" s g) ""
      [Gap0, Gap1, Gap2, Gap3, Gap4, Gap5, GapNeg1, GapNeg2, GapNeg3, GapNeg4, GapNeg5, GapAuto]
    p = foldr (\b a -> a <> [p' b]) [] [TopSide, BottomSide, LeftSide, RightSide, XSides, YSides, AllSides]

    d' s = foldr (shimDisplaySize s) "" [DisplayNone, DisplayInline, DisplayInlineBlock, DisplayBlock, DisplayTable, DisplayCell, DisplayRow, DisplayFlex, DisplayInlineFlex]
    d = foldr (\b a -> [d' b] <> a) [] [DisplayDefault, DisplayOnSmall, DisplayOnMedium, DisplayOnLarge, DisplayOnExtraLarge, DisplayOnPrint]
    t =
      [ ".text-capitalize { text-transform: capitalize; }"
      , ".text-lowercase { text-transform: lowercase; }"
      , ".text-uppercase { text-transform: uppercase; }"
      , ".text-truncate { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }"
      , ".text-wrap { white-space: normal !important; }"
      , ".text-nowrap { white-space: nowrap !important; }"
      , ".text-break { word-break: break-word !important; overflow-wrap: break-word !important; }"
      , ".text-reset { color: inherit !important; }"
      , ".text-decoration-none { text-decoration: none !important; }"
      ]
    flex =
      [ ".flex-column { flex-direction: column !important; }"]
    btn =
      [ ".btn-block { width: 100%;} "
      , ".btn-link {text-align: center; cursor:pointer; vertical-align: bottom;}"
      , ".btn-group { display: flex; align-items: stretch; }"
      , ".btn-group-vertical { display: flex; align-items: flex-start; flex-direction: column; }"
      , ".btn-group-vertical > .btn { align-self: stretch; }"
      ]
    float =
      [ ".float-right { float: right; }"
      , ".float-left { float: left; }"
      , ".float-none { float: none; }"
      ]
    sr =
      [ ".sr-only { position: absolute; width: 1px; height: 1px; padding: 0; margin: -1px; overflow: hidden; clip: rect(0, 0, 0, 0); border: 0; }"
      , ".sr-only-focusable:active, .sr-only-focusable:focus { position: static; width: auto; height: auto; margin: 0; overflow: visible; clip: auto; }"
      ]
    utl =
      [ ".clear-fix { clear: both; }"
      , ".visible { display: inherit; }"
      , ".dropdown-trigger:after { display: inline-block; width: 0; height:0; margin-left: 0.255em; vertical-height: 0.255em; content: \"\"; border-top: 0.3em solid; border-right: 0.3em solid transparent; border-bottom: 0; border-left: 0.3em solid transparent; }"
      , ".dropdown-header { display: block; padding: .5rem 1.5rem; margin-bottom: 0; font-size: .875rem; white-space: nowrap; cursor: default !important; }"
      , ".dropdown-header:hover { display: block; padding: .5rem 1.5rem; margin-bottom: 0; font-size: .875rem; white-space: nowrap; cursor: default !important; }"
      ]

shimDisplaySize :: DisplaySize -> DisplayOption -> T.Text -> T.Text
shimDisplaySize s o l =
  mediaFromDisplaySize s (".d-" <> textFromDisplaySize s <> textFromDisplayOption o <> "{" <> prop <> "}") <> l
  where
    prop = "display: " <> textFromDisplayOption o <> "!important;"

textFromDisplaySize :: DisplaySize -> T.Text
textFromDisplaySize = \case
  DisplayDefault -> ""
  DisplayOnSmall -> "sm-"
  DisplayOnMedium -> "md-"
  DisplayOnLarge -> "lg-"
  DisplayOnExtraLarge -> "xl-"
  DisplayOnPrint -> "print-"

textFromDisplayOption :: DisplayOption -> T.Text
textFromDisplayOption = \case
  DisplayNone -> "none"
  DisplayInline -> "inline"
  DisplayInlineBlock -> "inline-block"
  DisplayBlock -> "block"
  DisplayTable -> "table"
  DisplayCell -> "table-cell"
  DisplayRow -> "table-row"
  DisplayFlex -> "flex"
  DisplayInlineFlex -> "inline-flex"

mediaFromDisplaySize :: DisplaySize -> T.Text -> T.Text
mediaFromDisplaySize s t = case s of
  DisplayDefault -> t
  DisplayOnSmall -> "@media only screen and (max-width: 600px){" <> t <> "}"
  DisplayOnMedium -> "@media only screen and (min-width: 601px) and (max-width: 992px){" <> t <> "}"
  DisplayOnLarge -> "@media only screen and (min-width: 993px) and (max-width: 1200px){" <> t <> "}"
  DisplayOnExtraLarge -> "@media only screen and (min-width: 1201px){" <> t <> "}"
  DisplayOnPrint -> "@media only print{" <> t <> "}"

emFromGap :: GapOption -> T.Text
emFromGap = \case
  Gap0    -> "0.00em"
  Gap1    -> "0.25em"
  Gap2    -> "0.50em"
  Gap3    -> "0.75em"
  Gap4    -> "1.0em"
  Gap5    -> "1.25em"
  GapNeg1 -> "-0.25em"
  GapNeg2 -> "-0.50em"
  GapNeg3 -> "-0.75em"
  GapNeg4 -> "-1.00em"
  GapNeg5 -> "-1.25em"
  GapAuto -> "auto"

styleFromGap :: T.Text -> SideOption -> GapOption -> T.Text
styleFromGap a TopSide g = "{ " <> a <> "-top:" <> emFromGap g <> " !important; }"
styleFromGap a BottomSide g = "{ " <> a <> "-bottom:" <> emFromGap g <> " !important; }"
styleFromGap a LeftSide g = "{ " <> a <> "-left:" <> emFromGap g <> " !important; }"
styleFromGap a RightSide g = "{ " <> a <> "-right:" <> emFromGap g <> " !important; }"
styleFromGap a XSides g = "{ " <> a <> "-left:" <> emFromGap g <> " !important;" <> a <> "-right:" <> emFromGap g <> " !important; }"
styleFromGap a YSides g = "{ " <> a <> "-top:" <> emFromGap g <> " !important;" <> a <> "-bottom:" <> emFromGap g <> " !important; }"
styleFromGap a AllSides g = "{ " <> a <> ":" <> emFromGap g <> " !important; }"

alignments :: T.Text -> [T.Text]
alignments v =
  [ "." <> v <> "-start { " <> v <> ": flex-start; }"
  , "." <> v <> "-end { " <> v <> ": flex-end; }"
  , "." <> v <> "-center { " <> v <> ": flex-center; }"
  , "." <> v <> "-baseline { " <> v <> ": baseline; }"
  , "." <> v <> "-stretch { " <> v <> ": stretch; }"
  ]

percentKeyProperty :: T.Text -> T.Text -> [T.Text]
percentKeyProperty k p =
      [ "." <> k <> "-100 { " <> p <> ": 100%; }"
      , "." <> k <> "-75 { " <> p <> ": 75%; }"
      , "." <> k <> "-50 { " <> p <> ": 50%; }"
      , "." <> k <> "-25 { " <> p <> ": 25%; }"
      , "." <> k <> "-auto { " <> p <> ": auto; }"
      ]

percentKeyPropertySuffix :: T.Text -> T.Text -> T.Text -> [T.Text]
percentKeyPropertySuffix k p v =
  [ "." <> k <> "-100 { " <> p <> ": 100" <> v <> "; }"
  , "." <> k <> "-75 { " <> p <> ": 75" <> v <> "; }"
  , "." <> k <> "-50 { " <> p <> ": 50" <> v <> "; }"
  , "." <> k <> "-25 { " <> p <> ": 25" <> v <> "; }"
  , "." <> k <> "-auto { " <> p <> ": auto; }"
  ]
