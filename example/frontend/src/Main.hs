module Main where

import qualified Data.Map as Map
import Data.Text
import Reflex.Dom

import Reflex.Dom.RefleCSS.Bootstrap
main :: IO ()
main = mainWidgetWithHead headerBootstrap body

-- import Reflex.Dom.RefleCSS.Materialize
-- main :: IO ()
-- main = mainWidgetWithHead headerMaterialize body

headerBootstrap :: MonadWidget t m => m ()
headerBootstrap = do
  meta
  cssFile "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css"

  evPB <- getPostBuild
  dyEvJQuery <- widgetHold (return never) $ jsFile "https://code.jquery.com/jquery-3.2.1.slim.min.js" <$ evPB
  dyEvPopper <- widgetHold (return never) $ jsFile "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js" <$ switchPromptlyDyn dyEvJQuery
  _ <- widgetHold (return never) $ jsFile "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js" <$ switchPromptlyDyn dyEvPopper
  return ()

headerMaterialize :: MonadWidget t m => m ()
headerMaterialize = do
  meta
  cssFile "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/css/materialize.min.css"

  evPB <- getPostBuild
  _ <- widgetHold (return never) $ jsFile "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/js/materialize.min.js" <$ evPB
  return ()

cssFile :: MonadWidget t m => Text -> m ()
cssFile r = elAttr "link" (Map.fromList [
      ("rel", "stylesheet")
    , ("type", "text/css")
    , ("href", r)
  ]) $ return ()

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

body :: MonadWidget t m => m ()
body =
  rcContainer [] $ do
    rcRow [] $ do
      rcColAttr [(Medium, Break8)] [M TopSide NoSize Gap2] (Map.fromList [("id", "themeColors")]) $
        rcDiv [Row, TextAlign FloatOnAll TextCenter] $ do
          rcCol [(NoSize, Break6), (Small, Break3)] [P YSides NoSize Gap2] $
            rcBtn NormalButton PrimaryContext BlockButton [] $ text "Primary"
          rcCol [(NoSize, Break6), (Small, Break3)] [P YSides NoSize Gap2] $
            rcBtn NormalButton SecondaryContext BlockButton [] $ text "Secondary"
          rcCol [(NoSize, Break6), (Small, Break3)] [P YSides NoSize Gap2] $
            rcBtn NormalButton SuccessContext BlockButton [] $ text "Success"
          rcCol [(NoSize, Break6), (Small, Break3)] [P YSides NoSize Gap2] $
            rcBtn NormalButton DangerContext BlockButton [] $ text "Danger"
          rcCol [(NoSize, Break6), (Small, Break3)] [P YSides NoSize Gap2] $
            rcBtn NormalButton InfoContext BlockButton [] $ text "Info"
          rcCol [(NoSize, Break6), (Small, Break3)] [P YSides NoSize Gap2] $
            rcBtn NormalButton WarningContext BlockButton [] $ text "Warn"
          rcCol [(NoSize, Break6), (Small, Break3)] [P YSides NoSize Gap2] $
            rcBtn NormalButton LightContext BlockButton [] $ text "Light"
          rcCol [(NoSize, Break6), (Small, Break3)] [P YSides NoSize Gap2] $
            rcBtn NormalButton DarkContext BlockButton [] $ text "Dark"
      rcCol [(Medium, NoBreak)] [] blank
    rcRow [M TopSide NoSize Gap3] $ do
      rcCol [(NoSize, Break12)] [TextContext PrimaryContext] $ do
        rcSpan [Display DisplayDefault DisplayBlock, TextCapitalize] $ text "Capitalize: the fox jumped over the fence."
        rcSpan [Display DisplayDefault DisplayBlock, TextUppercase] $ text "Uppercase: the fox jumped over the fence."
        rcSpan [Display DisplayDefault DisplayBlock, TextLowercase] $ text "LOWERCASE: THE FOX JUMPED OVER THE FENCE."
      rcCol [(Medium, Break8)] [] $ do
        rcDiv [P BottomSide NoSize Gap3] $ do
          rcBtn NormalButton PrimaryContext MediumButton [M RightSide NoSize Gap1] $ text "Button"
          rcBtn OutlineButton PrimaryContext MediumButton [] $ text "Outline"
        rcDiv [P BottomSide NoSize Gap3] $ do
          rcBtn NormalButton PrimaryContext LargeButton [M RightSide NoSize Gap1, Align VerticalBottom] $ text "Large"
          rcBtn NormalButton PrimaryContext MediumButton [M RightSide NoSize Gap1, Align VerticalBottom] $ text "Medium"
          rcBtn NormalButton PrimaryContext SmallButton [M RightSide NoSize Gap1, Align VerticalBottom] $ text "Small"
          rcBtn LinkButton NoContext SmallButton [TextContext PrimaryContext] $ text "Link Button"

    rcRow [] $ do
      rcCol [(ExtraLarge, Break9)] [P BottomSide NoSize Gap4] $ do
        rcNavbar [ExpandMedium, DarkNavbar] [Bg PrimaryContext, M BottomSide NoSize Gap2] $ do
          rcNavBrand "#" [] $ text "Light text on bg-primary"
          rcNavToggle "#navbarColor02" [] []
          rcNavCollapse "navbarColor02" [] $
            rcNavbarNav [M LeftSide NoSize GapAuto] $ do
              rcNavItem [] $
                rcNavLink "#" "Current breakpoint tier" [] $ do
                  text "Tier"
                  rcSpan [Display DisplayOnExtraLarge DisplayInline, Display DisplayDefault DisplayNone] $ text "XL"
                  rcSpan [Display DisplayOnLarge DisplayInline, Display DisplayOnExtraLarge DisplayNone, Display DisplayDefault DisplayNone] $ text "LG"
                  rcSpan [Display DisplayOnMedium DisplayInline, Display DisplayOnLarge DisplayNone, Display DisplayDefault DisplayNone] $ text "MD"
                  rcSpan [Display DisplayOnSmall DisplayInline, Display DisplayOnMedium DisplayNone, Display DisplayDefault DisplayNone] $ text "SM"
                  rcSpan [Display DisplayDefault DisplayInline, Display DisplayOnSmall DisplayNone] $ text "XS"
              rcNavItem [] $
                rcNavLink "#" "" [] $ text "About"
        rcNavbar [ExpandMedium, LightNavbar] [Bg PrimaryContext, M BottomSide NoSize Gap3] $ do
          rcNavBrand "#" [] $ text "Dark text on bg-primary"
          rcNavToggle "#navbarColor03" [] []
          rcNavCollapse "navbarColor03" [] $
            rcNavbarNav [M LeftSide NoSize GapAuto] $ do
              rcNavItem [] $
                rcNavLink "#" "" [] $ text "Link"
              rcNavItem [] $
                rcNavLink "#" "" [] $ text "Demo"

        el "hr" blank

        rcNav [NavTabs] [] $ do
          rcNavItem [] $
            rcNavTab "#tab1" [Active] $ text "Tab"
          rcNavItem [] $
            rcNavTab "#tab2" [] $ text "Tab"
          rcNavItem [] $
            rcNavTab "#tab3" [] $ text "Tab"
          rcNavItem [] $
            rcNavTab "#tab4" [Disabled] $ text "Tab"
        rcTabContent [P XSides NoSize Gap1, P TopSide NoSize Gap3] $ do
          rcTabPane "tab1" [Active] $ do
            rcTag "p" [Lead] $ text "Bootstrap"
            text "Select a palette, change the "
            rcSpan [TextContext PrimaryContext] $ text "primary"
            text ", "
            rcSpan [TextContext SecondaryContext] $ text "secondary"
            text ", "
            rcSpan [TextContext SuccessContext] $ text "success"
            text ", "
            rcSpan [TextContext DangerContext] $ text "danger"
            text ", "
            rcSpan [TextContext InfoContext] $ text "info"
            text ", and "
            rcSpan [TextContext WarningContext] $ text "warning"
            text " colors."
            text "Choose from various Google Fonts, and modify most of the Bootstrap SASS variables. Click to generate your completely custom Bootstrap theme."
          rcTabPane "tab2" [] $
            text "The whole idea of this tool is to make the Bootstrap customization process easier, and allow you to visualize changes along the way. \
                 \For most users it's designed to be point-and-click, while advanced users can delve into the SASS as desired. It's a 4-step process."
          rcTabPane "tab3" [] $
            text "To know Bootstrap, is to know that there are a ton of SASS variables. Changing these variables is the very core of Bootstrap customization. \
                 \Even if you don't have any experience with SASS, you can use this to easily generate the CSS for your custom theme."
      rcCol [(ExtraLarge, Break3)] [P BottomSide NoSize Gap4] $ do
        rcNav [NavPills] [FlexColumn, M BottomSide NoSize Gap3] $ do
          rcNavItem [] $
            rcNavLinkAttr "#" "" [Active] (Map.fromList [("data-toggle", "popover"),("data-trigger","hover"),("data-placement","top"),("data-title","Primary color"), ("data-content","Links and 'active' elements always use the primary color."),("data-original-title","")]) $ text "Active"
          rcNavItem [] $
            rcNavLink "#" "" [] $ text "Link"
          rcNavItem [] $
            rcNavLink "#" "" [] $ text "Link"
          rcNavItem [] $
            rcNavLink "#" "" [] $ text "Link"
          rcNavItem [] $
            rcNavLink "#" "" [Disabled] $ text "Disabled"
        rcBtnGroup ButtonGroupMedium [M BottomSide NoSize Gap3, W Percent100] $ do
          rcBtn NormalButton PrimaryContext BlockButton [] $ text "Dropdown"
          rcToggleSplit NormalButton PrimaryContext MediumButton [] $
            rcSpan [SROnly] $ text "Toggle Dropdown"
          rcDropdownMenuRight NoSize [] $ do
            rcTag "h6" [DropdownHeader] $ text "Dropdown Header"
            rcDropdownItem "#" [] $ text "Action"
            rcDropdownItem "#" [] $ text "Link"
            rcDiv [DropdownDivider] blank
            rcDropdownItem "#" [] $ text "Separated Link"

    rcDiv [Display DisplayDefault DisplayFlex, M YSides NoSize Gap3] $
      rcDiv [Jumbotron, W Percent100, P YSides NoSize Gap5, M XSides NoSize GapAuto] $ do
        el "h1" $ text "Jumbotron"
        rcTag "p" [Lead] $ text "Rapidly build Bootstrap styled pages using Reflex DOM + RefleCSS"

    rcRow [] $ do
      rcCol [(ExtraLarge, Break6), (Large, Break4)] [M BottomSide NoSize Gap4] $
        rcCard [BorderContext PrimaryContext, H Percent100] $
          rcCardBody [Display DisplayDefault DisplayFlex, FlexColumn, AlignItems NoSize AlignStart] $ do
            rcTag "h4" [CardTitle, TextContext PrimaryContext] $ text "Card Title"
            rcTag "p" [CardText] $ text "Card Text: Lorem ipsum dolor sit amet, vitae \
                                        \malesuada id orci purus aliquip vivamus, \
                                        \at scelerisque a lectus justo a magna. \
                                        \Ac pede, sed nibh ornare, leo et libero, \
                                        \id cras at velit."
            rcBtn NormalButton PrimaryContext MediumButton [M TopSide NoSize GapAuto] $ text "Button"
      rcCol [(ExtraLarge, Break6), (Large, Break4)] [M BottomSide NoSize Gap4] $
        rcCard [BorderContext PrimaryContext, H Percent100] $
          rcCardBody [] $
            rcListGroup [] $ do
              rcListAction "#" NoContext [Active] $ text "Active"
              rcListAction "#" NoContext [] $ text "Option"
              rcListAction "#" NoContext [] $ text "Option"
              rcListAction "#" NoContext [Disabled] $ text "Disabled"
      rcCol [(ExtraLarge, Break12), (Large, Break4)] [M BottomSide NoSize Gap4] $
        rcCard [Bg PrimaryContext, TextContext WhiteContext, H Percent100] $
          rcCardBody [Display DisplayDefault DisplayFlex, FlexColumn, AlignItems NoSize AlignStart] $ do
            rcTag "h4" [CardTitle] $ text "Theme vs. Template"
            rcTag "p" [CardText] $ text "In context, \"Theme\" refers to the style layer or \"skin\". The \"Theme\" encompasses colors, fonts and style. A \"Template\" refers to a page layout and its' HTML structure."
            rcLinkBtn NormalButton PrimaryContext MediumButton [BorderContext WhiteContext, M TopSide NoSize GapAuto] $ text "Button"

    rcRow [] $ do
      rcCol [(Medium, Break12)] [] $
        rcBtnAttr NormalButton PrimaryContext MediumButton [] (Map.fromList[("data-toggle","modal"),("data-target","#modalDemo")]) $ text "Show Modal"
      rcCol [(Medium, Break12)] [] $
        rcContainerFluid [] $
          rcModal "modalDemo" [] $
            rcModalDialog MediumModal [] [] $
              rcModalContent [] $ do
                rcModalHeader [] $ do
                  rcTag "h5" [ModalTitle] $ text "Modal Title"
                  rcCloseModal []
                rcModalBody [] $
                  el "p" $ text "Modal body goes here."
                rcModalFooter [] $ do
                  rcBtnAttr NormalButton SecondaryContext MediumButton [] (Map.fromList[("data-dismiss", "modal")]) $ text "Close"
                  rcBtn NormalButton PrimaryContext MediumButton [] $ text "Save changes"

    rcRow [M TopSide NoSize Gap2] $ do
      rcCol [(ExtraLarge, Break4)] [] $ do
        el "h6" $ text "Heading 6"
        el "h5" $ text "Heading 6"
        el "h4" $ text "Heading 6"
        rcTag "h3" [TextTruncate] $ text "Heading 3"
        rcTag "h2" [TextTruncate] $ text "Heading 2"
        rcTag "h1" [TextTruncate] $ text "Heading 1"
        rcTag "p" [Lead] $ text "Lead"
        el "p" $ text "Paragraph"
      rcCol [(ExtraLarge, Break8)] [TextAlign FloatOnAll TextRight] $ do
        rcTag "p" [Display4, TextTruncate] $ text "Display 4"
        rcTag "p" [Display3, TextTruncate] $ text "Display 3"
        rcTag "p" [Display2, TextTruncate] $ text "Display 2"
        rcTag "p" [Display1, TextTruncate] $ text "Display 1"

    rcRow [M TopSide NoSize Gap2] $
      rcCol [(Medium, Break12)] [] $
        rcForm [] $ do
          rcDiv [FormGroup, Row] $ do
            rcTag "label" [Col Medium Break2, ColFormLabel NoSize, FormControlLabel] $
              text "First"
            rcDiv [Col Medium Break4] $
              rcTextInput "" "Jane" []
            rcTag "label" [Col Medium Break1, ColFormLabel NoSize, FormControlLabel] $
              text "Last"
            rcDiv [Col Medium Break5] $
              rcTextInput "" "Doe" []
          rcDiv [FormGroup, Row] $ do
            rcTag "label" [Col Large Break2, ColFormLabel NoSize, FormControlLabel] $
              text "Address"
            rcDiv [Col Large Break10] $
              rcTextInput "Street" "" []
          rcDiv [FormGroup, Row] $ do
            rcTag "label" [Col Large Break2, ColFormLabel NoSize, FormControlLabel] blank
            rcDiv [Col Large Break6] $
              rcTextInput "City" "" []
            rcDiv [Col Large Break4] $
              -- TODO: Implement Select Helper
              rcTag "select" [FormControl NoSize] $ do
                elAttr "option" (Map.fromList [("value","State")]) $ text "State"
                elAttr "option" (Map.fromList [("value","AK")]) $ text "AK"
                elAttr "option" (Map.fromList [("value","AZ")]) $ text "AZ"
                elAttr "option" (Map.fromList [("value","CA")]) $ text "CA"
                elAttr "option" (Map.fromList [("value","CO")]) $ text "CO"
          rcDiv [FormGroup, Row, Custom ["was-validated"]] $ do
            rcTag "label" [Col Large Break2, ColFormLabel NoSize, FormControlLabel] $
              text "Username"
            rcDiv [Col Large Break10] $ do
              rcTextInputAttr "" "" [] (Map.fromList [("required","")])
              rcDiv [InvalidFeedback] $ text "Username is required"
          rcDiv [FormGroup, Row] $ do
            rcTag "label" [Col Large Break2, ColFormLabel NoSize, FormControlLabel] blank
            rcDiv [Col Large Break10] $ do
              rcBtn OutlineButton SecondaryContext MediumButton [M RightSide NoSize Gap2] $ text "Cancel"
              rcBtn NormalButton PrimaryContext MediumButton [M RightSide NoSize Gap2] $ text "Submit"
              rcDiv [Custom ["custom-control", "custom-checkbox"], Display DisplayDefault DisplayInline] $ do
                -- TODO: Implement Checkbox helper
                rcTagAttr "input" [Custom ["custom-control-input"]] (Map.fromList [("type","checkbox"),("id","customCheck"),("checked","")]) blank
                rcTagAttr "label" [Custom ["custom-control-label"]] (Map.fromList [("for","customCheck")]) $ text "Check me out"
    rcTag "p" [P YSides NoSize Gap3] $
      text "The rows & columns of the world famous Bootstrap grid speak for themselves."
    rcRow [] $ do
      rcCol [(Small, Break6)] [] $ text "6"
      rcCol [(NoSize, Break6)] [] $
        rcCard [M YSides NoSize Gap1, Bg LightContext] $ text "6"
      rcCol [(Small, Break5)] [] $ text "5"
      rcCol [(NoSize, Break7)] [] $
        rcCard [M YSides NoSize Gap1] $ text "7"
      rcCol [(Small, Break4)] [] $ text "4"
      rcCol [(NoSize, Break8)] [] $
        rcCard [M YSides NoSize Gap1] $ text "8"
      rcCol [(Small, Break3)] [] $ text "3"
      rcCol [(NoSize, Break9)] [] $
        rcCard [M YSides NoSize Gap1] $ text "9"
      rcCol [(Small, Break2)] [] $ text "2"
      rcCol [(NoSize, Break10)] [] $
        rcCard [M YSides NoSize Gap1] $ text "10"
      rcCol [(Small, Break1)] [] $ text "1"
      rcCol [(NoSize, Break11)] [] $
        rcCard [M YSides NoSize Gap1] $ text "11"

    -- TODO: Implement Table Helpers
    rcRow [M TopSide NoSize Gap2] $
      rcCol [(Medium, Break12)] [] $
        rcDiv [TableResponsive NoSize] $
          rcTag "table" [Table [TableHover, TableSmall]] $
            rcTag "tbody" [] $ do
              rcTag "tr" [] $ do
                rcTag "th" [W Percent25] $ text "Location"
                rcTag "th" [W Percent50] $ text "Date"
                rcTag "th" [W Percent25] $ text "Visits"
              rcTag "tr" [] $ do
                rcTag "td" [] $ text "Westfield"
                rcTag "td" [] $ do
                  text "08.05.18"
                  rcSpan [Badge PrimaryContext] $ text "badge"
                rcTag "td" [] $ text "2323"
              rcTag "tr" [] $ do
                rcTag "td" [] $ text "Galway"
                rcTag "td" [] $ do
                  text "08.05.18"
                  rcSpan [BadgePill PrimaryContext] $ text "badge-pill"
                rcTag "td" [] $ text "5362"
              rcTag "tr" [TableContext PrimaryContext] $ do
                rcTag "td" [] $ text "Bern"
                rcTag "td" [] $ text "08.05.18"
                rcTag "td" [] $ text "153"
    -- TODO: Implement Alert Helpers
    rcRow [M TopSide NoSize Gap2] $
      rcCol [(NoSize, Break12)] [] $
        rcDivAttr [Alert PrimaryContext, AlertDismissable, Show, Fade] (Map.fromList [("role","alert")]) $ do
          rcCloseAlert []
          text "Look at this amazing alert!"
    return ()
