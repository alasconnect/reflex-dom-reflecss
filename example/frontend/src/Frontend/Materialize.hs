module Frontend.Materialize where

import qualified Data.Map as Map
import Language.Javascript.JSaddle
import Reflex.Dom

import Reflex.Dom.RefleCSS.Materialize

header :: MonadWidget t m => m ()
header = do
  loadResources
  return ()

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
      rcCol [(NoSize, Break12)] [] $ do
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
          rcLinkBtn [] $ text "Link Button"

    rcRow [] $ do
      rcCol [(ExtraLarge, Break9)] [P BottomSide NoSize Gap4] $ do
        rcNavbar [ExpandMedium, DarkNavbar] [Bg PrimaryContext, M BottomSide NoSize Gap2] $ do
          rcNavBrand "javascript:void(0)" [TextTruncate] $ text "Light text on default bg"
          rcNavToggle "navbarColor02" [] []
          rcNavCollapse "navbarColor02" [] $
            rcNavbarNav [M LeftSide NoSize GapAuto] $ do
              rcNavItem [] $
                rcNavLink "javascript:void(0)" "Current breakpoint tier" [] $ do
                  text "Tier"
                  rcSpan [ Display DisplayOnExtraLarge DisplayInline
                         , Display DisplayOnLarge DisplayNone
                         , Display DisplayOnMedium DisplayNone
                         , Display DisplayOnSmall DisplayNone
                         , Display DisplayDefault DisplayNone
                         ] $ text "XL"
                  rcSpan [ Display DisplayOnExtraLarge DisplayNone
                         , Display DisplayOnLarge DisplayInline
                         , Display DisplayOnMedium DisplayNone
                         , Display DisplayOnSmall DisplayNone
                         , Display DisplayDefault DisplayNone
                         ] $ text "LG"
                  rcSpan [ Display DisplayOnExtraLarge DisplayNone
                         , Display DisplayOnLarge DisplayNone
                         , Display DisplayOnMedium DisplayInline
                         , Display DisplayOnSmall DisplayNone
                         , Display DisplayDefault DisplayNone
                         ] $ text "MD"
                  rcSpan [ Display DisplayOnExtraLarge DisplayNone
                         , Display DisplayOnLarge DisplayNone
                         , Display DisplayOnMedium DisplayNone
                         , Display DisplayOnSmall DisplayInline
                         , Display DisplayDefault DisplayNone
                         ] $ text "SM"
                  rcSpan [ Display DisplayOnExtraLarge DisplayNone
                         , Display DisplayOnLarge DisplayNone
                         , Display DisplayOnMedium DisplayNone
                         , Display DisplayOnSmall DisplayNone
                         , Display DisplayDefault DisplayInline
                         ] $ text "XS"
              rcNavItem [] $
                rcNavLink "javascript:void(0)" "" [] $ text "About"
        rcNavbar [ExpandMedium, LightNavbar] [Bg PrimaryContext, M BottomSide NoSize Gap3] $ do
          rcNavBrand "javascript:void(0)" [TextTruncate] $ text "Dark text on bg-primary"
          rcNavToggle "navbarColor03" [] []
          rcNavCollapse "navbarColor03" [] $
            rcNavbarNav [M LeftSide NoSize GapAuto] $ do
              rcNavItem [] $
                rcNavLink "javascript:void(0)" "" [] $ text "Link"
              rcNavItem [] $
                rcNavLink "javascript:void(0)" "" [] $ text "Demo"

        el "hr" blank

        rcTabBar [] $ do
          rcTabAction "#tab1" [Active] $ text "Tab"
          rcTabAction "#tab2" [] $ text "Tab"
          rcTabAction "#tab3" [] $ text "Tab"
          rcTabAction "#tab4" [Disabled] $ text "Tab"
        rcTabContent [P XSides NoSize Gap1, P TopSide NoSize Gap3] $ do
          rcTabPane "tab1" [Active] $ do
            rcTag "p" [Lead] $ text "Bootstrap"
            text "Select a palette, change the "
            rcSpan [TextContext PrimaryContext, Custom ["black-text"]] $ text "primary"
            text ", "
            rcSpan [TextContext SecondaryContext, Custom ["blue-text"]] $ text "secondary"
            text ", "
            rcSpan [TextContext SuccessContext, Custom ["green-text"]] $ text "success"
            text ", "
            rcSpan [TextContext DangerContext, Custom ["red-text"]] $ text "danger"
            text ", "
            rcSpan [TextContext InfoContext, Custom ["teal-text"]] $ text "info"
            text ", and "
            rcSpan [TextContext WarningContext, Custom ["orange-text"]] $ text "warning"
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
            rcNavLinkAttr "javascript:void(0)" "" [Active] (Map.fromList [("data-toggle", "popover"),("data-trigger","hover"),("data-placement","top"),("data-title","Primary color"), ("data-content","Links and 'active' elements always use the primary color."),("data-original-title","")]) $ text "Active"
          rcNavItem [] $
            rcNavLink "javascript:void(0)" "" [] $ text "Link"
          rcNavItem [] $
            rcNavLink "javascript:void(0)" "" [] $ text "Link"
          rcNavItem [] $
            rcNavLink "javascript:void(0)" "" [] $ text "Link"
          rcNavItem [] $
            rcNavLink "javascript:void(0)" "" [Disabled] $ text "Disabled"
        rcBtnGroup ButtonGroupMedium [M BottomSide NoSize Gap3, W Percent100] $ do
          rcBtn NormalButton PrimaryContext BlockButton [] $ text "Dropdown"
          rcToggleSplit "ddm" NormalButton PrimaryContext MediumButton [] $
            rcSpan [SROnly] $ text "Toggle Dropdown"
          rcDropdownMenuRight "ddm" NoSize [] $ do
            rcDropdownHeader [] $ text "Dropdown Header"
            rcDropdownItem "javascript:void(0)" [] $ text "Action"
            rcDropdownItem "javascript:void(0)" [] $ text "Link"
            rcDropdownDivider []
            rcDropdownItem "javascript:void(0)" [] $ text "Separated Link"

    rcDiv [Display DisplayDefault DisplayFlex, M YSides NoSize Gap3] $
      rcDiv [Jumbotron, W Percent100, P YSides NoSize Gap5, M XSides NoSize GapAuto] $ do
        el "h1" $ text "Jumbotron"
        rcTag "p" [Lead] $ text "Rapidly build Bootstrap styled pages using Reflex DOM + RefleCSS"

    rcRow [] $ do
      rcCol [(ExtraLarge, Break6), (Large, Break4)] [M BottomSide NoSize Gap4] $
        rcCard [BorderContext PrimaryContext, H Percent100] $ do
          rcCardBody [Display DisplayDefault DisplayFlex, FlexColumn, AlignItems NoSize AlignStart] $ do
            rcTag "h4" [CardTitle, TextContext PrimaryContext] $ text "Card Title"
            rcTag "p" [CardText] $ text "Card Text: Lorem ipsum dolor sit amet, vitae \
                                        \malesuada id orci purus aliquip vivamus, \
                                        \at scelerisque a lectus justo a magna. \
                                        \Ac pede, sed nibh ornare, leo et libero, \
                                        \id cras at velit."
          rcDiv [CardFooter] $
            rcBtn NormalButton PrimaryContext MediumButton [M TopSide NoSize GapAuto] $ text "Button"
      rcCol [(ExtraLarge, Break6), (Large, Break4)] [M BottomSide NoSize Gap4] $
        rcCard [BorderContext PrimaryContext, H Percent100] $
          rcCardBody [] $
            rcListGroup [] $ do
              rcListAction "javascript:void(0)" NoContext [Active] $ text "Active"
              rcListAction "javascript:void(0)" NoContext [] $ text "Option"
              rcListAction "javascript:void(0)" NoContext [] $ text "Option"
              rcListAction "javascript:void(0)" NoContext [Disabled] $ text "Disabled"
      rcCol [(ExtraLarge, Break12), (Large, Break4)] [M BottomSide NoSize Gap4] $
        rcCard [Bg PrimaryContext, TextContext WhiteContext, H Percent100] $
          rcCardBody [Display DisplayDefault DisplayFlex, FlexColumn, AlignItems NoSize AlignStart] $ do
            rcTag "h4" [CardTitle] $ text "Theme vs. Template"
            rcTag "p" [CardText] $ text "In context, \"Theme\" refers to the style layer or \"skin\". The \"Theme\" encompasses colors, fonts and style. A \"Template\" refers to a page layout and its' HTML structure."
            rcLinkBtn [BorderContext WhiteContext, M TopSide NoSize GapAuto] $ text "Button"

    rcRow [] $ do
      rcCol [(Medium, Break12)] [] $
        rcShowModalBtn "modalDemo" NormalButton PrimaryContext MediumButton [] $ text "Show Modal"
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
        el "h5" $ text "Heading 5"
        el "h4" $ text "Heading 4"
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
      rcCol [(NoSize, Break12)] [] $
        rcForm [WasValidated] $ do
          rcFormGroupRow $ do
            rcInputField TextField "firstname" "First" "" "Jane" [Col NoSize Break6] [] []
            rcInputField TextField "lastname" "Last" "" "Doe" [Col NoSize Break6] [] []
          rcFormGroupRow $
            rcInputField TextField "address" "Address" "Street" "" [Col NoSize Break12] [] []
          rcFormGroupRow $ do
            rcInputField TextField "city" "City" "City" "" [Col NoSize Break8] [] []
            rcDiv [Col NoSize Break4] $ do
              -- TODO: Implement Select Helper
              rcLabel "state" "State" []
              rcTagAttr "select" [FormControl NoSize] (Map.fromList [("id", "state")]) $ do
                elAttr "option" (Map.fromList [("value","State")]) $ text "State"
                elAttr "option" (Map.fromList [("value","AK")]) $ text "AK"
                elAttr "option" (Map.fromList [("value","AZ")]) $ text "AZ"
                elAttr "option" (Map.fromList [("value","CA")]) $ text "CA"
                elAttr "option" (Map.fromList [("value","CO")]) $ text "CO"
          rcFormGroupRow $
            rcInputFieldV PhoneField "phone" "Phone" "555-555-5555" ""
              ""
              "Phone number is invalid."
              [Col NoSize Break12]
              []
              []
              (Map.fromList [("required",""),("pattern","[0-9]{3}-[0-9]{3}-[0-9]{4}")])
          rcFormGroupRow $
            rcInputFieldV TextField "username" "Username" "" ""
              ""
              "Username is required."
              [Col NoSize Break12]
              []
              []
              (Map.fromList [("required","")])
          rcFormGroupRow $ do
            rcTag "label" [Col NoSize Break2, ColFormLabel NoSize, FormControlLabel] blank
            rcDiv [Col NoSize Break6] $ do
              rcBtn OutlineButton SecondaryContext MediumButton [M RightSide NoSize Gap2] $ text "Cancel"
              rcBtn NormalButton PrimaryContext MediumButton [M RightSide NoSize Gap2] $ text "Submit"
            rcDiv [Col NoSize Break4] $
              rcCheckbox False "aCheckbox" "Check me out" []

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

    rcRow [M TopSide NoSize Gap2] $
      rcCol [(NoSize, Break12)] [] $
        rcAlert PrimaryContext [] $
          text "Look at this amazing alert!"

    evPB <- delay 1 =<< getPostBuild
    performEvent_ (liftJSM domChangeHook <$ evPB)

    return ()
