{-# LANGUAGE
    FlexibleContexts
#-}

{-|
  Module      : Reflex.Dom.RefleCSS.Bootstrap.V4.Dom
  Description : Helpers for building Bootstrap 4 widgets.
-}

module Reflex.Dom.RefleCSS.Bootstrap.V4.Dom where

-- Base --
-- Third Party --
import qualified Data.Map as M
import Data.Text hiding ( foldr )
import Language.Javascript.JSaddle
import Reflex.Dom
import Reflex.Dom.Builder.Class ( mapKeysToAttributeName )
-- Local --
import Reflex.Dom.RefleCSS.Bootstrap.V4.Class
import Reflex.Dom.RefleCSS.Class
import Reflex.Dom.RefleCSS.Util
-- End Imports --

loadResources :: MonadWidget t m => m ()
loadResources = do
  evPB <- getPostBuild

  meta
  dyEvBS <- widgetHold (return never) $ cssFile "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css" <$ evPB
  dyEvJQuery <- widgetHold (return never) $ jsFile "https://code.jquery.com/jquery-3.2.1.slim.min.js" <$ switchPromptlyDyn dyEvBS
  dyEvPopper <- widgetHold (return never) $ jsFile "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js" <$ switchPromptlyDyn dyEvJQuery
  _ <- widgetHold (return never) $ jsFile "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js" <$ switchPromptlyDyn dyEvPopper
  return ()

domChangeHook :: JSM ()
domChangeHook = return ()

rcTag :: MonadWidget t m => Text -> [Class] -> m a -> m a
rcTag t l = rcTagAttr t l M.empty
rcTagAttr :: MonadWidget t m => Text -> [Class] -> AttributeMap -> m a -> m a
rcTagAttr t l a = elAttr t (attr a l)

rcDiv :: MonadWidget t m => [Class] -> m a -> m a
rcDiv l = rcDivAttr l M.empty
rcDivAttr :: MonadWidget t m => [Class] -> AttributeMap -> m a -> m a
rcDivAttr = rcTagAttr "div"

rcSpan :: MonadWidget t m => [Class] -> m a -> m a
rcSpan l = rcSpanAttr l M.empty
rcSpanAttr :: MonadWidget t m => [Class] -> AttributeMap -> m a -> m a
rcSpanAttr = rcTagAttr "span"

rcContainer :: MonadWidget t m => [Class] -> m a -> m a
rcContainer l = rcContainerAttr l M.empty
rcContainerAttr :: MonadWidget t m => [Class] -> AttributeMap -> m a -> m a
rcContainerAttr l = rcDivAttr ([Container] <> l)

rcContainerFluid :: MonadWidget t m => [Class] -> m a -> m a
rcContainerFluid l = rcContainerFluidAttr l M.empty
rcContainerFluidAttr :: MonadWidget t m => [Class] -> AttributeMap -> m a -> m a
rcContainerFluidAttr l = rcDivAttr ([ContainerFluid] <> l)

rcRow :: MonadWidget t m => [Class] -> m a -> m a
rcRow l = rcRowAttr l M.empty
rcRowAttr :: MonadWidget t m => [Class] -> AttributeMap -> m a -> m a
rcRowAttr l = rcDivAttr ([Row] <> l)

rcTabBar :: MonadWidget t m => [Class] -> m a -> m a
rcTabBar l = rcTabBarAttr l M.empty
rcTabBarAttr :: MonadWidget t m => [Class] -> AttributeMap -> m a -> m a
rcTabBarAttr = rcNavAttr [NavTabs]

rcNav :: MonadWidget t m => [NavOption] -> [Class] -> m a -> m a
rcNav n l = rcNavAttr n l M.empty
rcNavAttr :: MonadWidget t m => [NavOption] -> [Class] -> AttributeMap -> m a -> m a
rcNavAttr n l = rcTagAttr "ul" ([Nav n] <> l)

rcNavbar :: MonadWidget t m => [NavbarOption] -> [Class] -> m a -> m a
rcNavbar n l = rcNavbarAttr n l M.empty
rcNavbarAttr :: MonadWidget t m => [NavbarOption] -> [Class] -> AttributeMap -> m a -> m a
rcNavbarAttr n l = rcTagAttr "div" ([Navbar n] <> l)

rcNavBrand :: MonadWidget t m => Text -> [Class] -> m a -> m (Event t ())
rcNavBrand r l = rcNavBrandAttr r l M.empty
rcNavBrandAttr :: MonadWidget t m => Text -> [Class] -> AttributeMap -> m a -> m (Event t ())
rcNavBrandAttr r l a w = do
  (e, _) <- elAttr' "a" (attr (M.insert "href" r a) ([NavbarBrand] <> l)) w
  return $ domEvent Click e

rcNavToggle :: MonadWidget t m => Text -> [Class] -> [Class] -> m (Event t ())
rcNavToggle t l l' = rcNavToggleAttr t l l' M.empty
rcNavToggleAttr :: MonadWidget t m => Text -> [Class] -> [Class] -> AttributeMap -> m (Event t ())
rcNavToggleAttr t l l' a = do
  (e, _) <-
    elAttr' "button" (attr (inject a [("type", "button"),("data-toggle","collapse"),("data-target", "#" <> t),("aria-expanded", "false"), ("aria-label", "Toggle navigation")]) ([NavbarToggler] <> l)) $
      elAttr "span" (mkAttr ([NavbarTogglerIcon] <> l')) blank
  return $ domEvent Click e

rcNavCollapse :: MonadWidget t m => Text -> [Class] -> m a -> m a
rcNavCollapse i l = rcNavCollapseAttr i l M.empty
rcNavCollapseAttr :: MonadWidget t m => Text -> [Class] -> AttributeMap -> m a -> m a
rcNavCollapseAttr i l a = rcDivAttr ([Collapse, NavbarCollapse] <> l) (inject a [("id", i)])

rcNavbarNav :: MonadWidget t m => [Class] -> m a -> m a
rcNavbarNav l = rcNavbarNavAttr l M.empty
rcNavbarNavAttr :: MonadWidget t m => [Class] -> AttributeMap -> m a -> m a
rcNavbarNavAttr l = rcTagAttr "ul" ([NavbarNav] <> l)

rcTabAction :: MonadWidget t m => Text -> [Class] -> m a -> m (Event t ())
rcTabAction t l = rcTabActionAttr t l M.empty
rcTabActionAttr :: MonadWidget t m => Text -> [Class] -> AttributeMap -> m a -> m (Event t ())
rcTabActionAttr t l a w = do
  (e, _) <- rcTag "li" ([NavItem] <> l) $ elAttr' "a" (attr (inject a [("href", t),("data-toggle", "tab")]) ([NavLink] <> l)) w
  return $ domEvent Click e

rcNavItem :: MonadWidget t m => [Class] -> m a -> m a
rcNavItem l = rcNavItemAttr l M.empty
rcNavItemAttr :: MonadWidget t m => [Class] -> AttributeMap -> m a -> m a
rcNavItemAttr l = rcTagAttr "li" ([NavItem] <> l)

rcNavLink :: MonadWidget t m => Text -> Text -> [Class] -> m a -> m (Event t ())
rcNavLink r t l = rcNavLinkAttr r t l M.empty
rcNavLinkAttr :: MonadWidget t m => Text -> Text -> [Class] -> AttributeMap -> m a -> m (Event t ())
rcNavLinkAttr r t l a w = do
  (e, _) <- elAttr' "a" (attr (inject a [("href", r),("title", t)]) ([NavLink] <> l)) w
  return $ domEvent Click e

rcTabContent :: MonadWidget t m => [Class] -> m a -> m a
rcTabContent l = rcTabContentAttr l M.empty
rcTabContentAttr :: MonadWidget t m => [Class] -> AttributeMap -> m a -> m a
rcTabContentAttr l = rcDivAttr ([TabContent] <> l)

rcTabPane :: MonadWidget t m => Text -> [Class] -> m a -> m a
rcTabPane i l = rcTabPaneAttr i l M.empty
rcTabPaneAttr :: MonadWidget t m => Text -> [Class] -> AttributeMap -> m a -> m a
rcTabPaneAttr i l a = rcDivAttr ([TabPane] <> l) (inject a [("role", "tabpanel"),("id", i)])

rcCol :: MonadWidget t m => [(SizeOption, BreakOption)] -> [Class] -> m a -> m a
rcCol t l = rcColAttr t l M.empty
rcColAttr :: MonadWidget t m => [(SizeOption, BreakOption)] -> [Class] -> AttributeMap -> m a -> m a
rcColAttr t l = rcDivAttr build
  where
    build = l <> fmap (uncurry Col) t

rcBtnGroup :: MonadWidget t m => ButtonGroupSizeOption -> [Class] -> m a -> m a
rcBtnGroup s l = rcBtnGroupAttr s l M.empty
rcBtnGroupAttr :: MonadWidget t m => ButtonGroupSizeOption -> [Class] -> AttributeMap -> m a -> m a
rcBtnGroupAttr s l = rcDivAttr ([BtnGroup s] <> l)

rcShowModalBtn :: MonadWidget t m => Text -> ButtonType -> ContextOption -> ButtonSizeOption -> [Class] -> m a -> m (Event t ())
rcShowModalBtn i t c s l = rcShowModalBtnAttr i t c s l M.empty
rcShowModalBtnAttr :: MonadWidget t m => Text -> ButtonType -> ContextOption -> ButtonSizeOption -> [Class] -> AttributeMap -> m a -> m (Event t ())
rcShowModalBtnAttr i t c s l a =
  rcBtnAttr t c s l (inject a [("data-toggle","modal"),("data-target","#" <> i)])

rcBtn :: MonadWidget t m => ButtonType -> ContextOption -> ButtonSizeOption -> [Class] -> m a -> m (Event t ())
rcBtn t c s l = rcBtnAttr t c s l M.empty
rcBtnAttr :: MonadWidget t m => ButtonType -> ContextOption -> ButtonSizeOption -> [Class] -> AttributeMap -> m a -> m (Event t ())
rcBtnAttr t c s l a w = do
  (e, _) <- elAttr' "button" (attr (inject a [("type", "button")]) ([Btn t c s] <> l)) w
  return $ domEvent Click e

rcLinkBtn :: MonadWidget t m => [Class] -> m a -> m (Event t ())
rcLinkBtn l = rcLinkBtnAttr l M.empty
rcLinkBtnAttr :: MonadWidget t m => [Class] -> AttributeMap -> m a -> m (Event t ())
rcLinkBtnAttr l a w = do
  (e, _) <- elAttr' "a" (attr (inject a [("href", "javascript:void(0)")]) l) w
  return $ domEvent Click e

rcToggleSplit :: MonadWidget t m => Text -> ButtonType -> ContextOption -> ButtonSizeOption -> [Class] -> m a -> m (Event t ())
rcToggleSplit t b c s l = rcToggleSplitAttr t b c s l M.empty
rcToggleSplitAttr :: MonadWidget t m => Text -> ButtonType -> ContextOption -> ButtonSizeOption -> [Class] -> AttributeMap -> m a -> m (Event t ())
rcToggleSplitAttr _ b c s l a =
  rcBtnAttr b c s ([DropdownToggleSplit] <> l) (inject a [("data-toggle", "dropdown"),("aria-haspopup","true"),("aria-expanded","false")])

rcToggle :: MonadWidget t m => Text -> ButtonType -> ContextOption -> ButtonSizeOption -> [Class] -> m a -> m (Event t ())
rcToggle t b c s l = rcToggleAttr t b c s l M.empty
rcToggleAttr :: MonadWidget t m => Text -> ButtonType -> ContextOption -> ButtonSizeOption -> [Class] -> AttributeMap -> m a -> m (Event t ())
rcToggleAttr _ b c s l a =
  rcBtnAttr b c s ([DropdownToggle] <> l) (inject a [("data-toggle", "dropdown"),("aria-haspopup","true"),("aria-expanded","false")])

rcDropdownMenu :: MonadWidget t m => Text -> [Class] -> m a -> m a
rcDropdownMenu i l = rcDropdownMenuAttr i l M.empty
rcDropdownMenuAttr :: MonadWidget t m => Text -> [Class] -> AttributeMap -> m a -> m a
rcDropdownMenuAttr i l a = rcDivAttr ([DropdownMenu] <> l) (inject a [("id", i)])

rcDropdownMenuRight :: MonadWidget t m => Text -> SizeOption -> [Class] -> m a -> m a
rcDropdownMenuRight i s l = rcDropdownMenuRightAttr i s l M.empty
rcDropdownMenuRightAttr :: MonadWidget t m => Text -> SizeOption -> [Class] -> AttributeMap -> m a -> m a
rcDropdownMenuRightAttr i s l a = rcDivAttr ([DropdownMenuRight s] <> l) (inject a [("id",i)])

rcDropdownMenuLeft :: MonadWidget t m => Text -> SizeOption -> [Class] -> m a -> m a
rcDropdownMenuLeft i s l = rcDropdownMenuLeftAttr i s l M.empty
rcDropdownMenuLeftAttr :: MonadWidget t m => Text -> SizeOption -> [Class] -> AttributeMap -> m a -> m a
rcDropdownMenuLeftAttr i s l a = rcDivAttr ([DropdownMenuLeft s] <> l) (inject a [("id",i)])

rcDropdownHeader :: MonadWidget t m => [Class] -> m a -> m a
rcDropdownHeader l = rcDropdownHeaderAttr l M.empty
rcDropdownHeaderAttr :: MonadWidget t m => [Class] -> AttributeMap -> m a -> m a
rcDropdownHeaderAttr l = rcTagAttr "h6" [DropdownHeader]

rcDropdownDivider :: MonadWidget t m => [Class] -> m ()
rcDropdownDivider l = rcDropdownDividerAttr l M.empty
rcDropdownDividerAttr :: MonadWidget t m => [Class] -> AttributeMap -> m ()
rcDropdownDividerAttr l a = rcDivAttr ([DropdownDivider] <> l) a blank

rcDropdownItem :: MonadWidget t m => Text -> [Class] -> m a -> m (Event t ())
rcDropdownItem t l = rcDropdownItemAttr t l M.empty
rcDropdownItemAttr :: MonadWidget t m => Text -> [Class] -> AttributeMap -> m a -> m (Event t ())
rcDropdownItemAttr r l a w = do
  (e, _) <- elAttr' "a" (attr (inject a [("href", r)]) ([DropdownItem] <> l)) w
  return $ domEvent Click e

rcCard :: MonadWidget t m => [Class] -> m a -> m a
rcCard l = rcCardAttr l M.empty
rcCardAttr :: MonadWidget t m => [Class] -> AttributeMap -> m a -> m a
rcCardAttr l = rcDivAttr ([Card] <> l)

rcCardBody :: MonadWidget t m => [Class] -> m a -> m a
rcCardBody l = rcCardBodyAttr l M.empty
rcCardBodyAttr :: MonadWidget t m => [Class] -> AttributeMap -> m a -> m a
rcCardBodyAttr l = rcDivAttr ([CardBody] <> l)

rcListGroup :: MonadWidget t m => [Class] -> m a -> m a
rcListGroup l = rcListGroupAttr l M.empty
rcListGroupAttr :: MonadWidget t m => [Class] -> AttributeMap -> m a -> m a
rcListGroupAttr l = rcDivAttr ([ListGroup] <> l)

rcListAction :: MonadWidget t m => Text -> ContextOption -> [Class] -> m a -> m (Event t ())
rcListAction r c l = rcListActionAttr r c l M.empty
rcListActionAttr :: MonadWidget t m => Text -> ContextOption -> [Class] -> AttributeMap -> m a -> m (Event t ())
rcListActionAttr r c l a w = do
  (e, _) <- elAttr' "a" (attr (inject a [("href", r)]) ([ListGroupItem c, ListGroupItemAction] <> l)) w
  return $ domEvent Click e

rcModal :: MonadWidget t m => Text -> [Class] -> m a -> m a
rcModal i l = rcModalAttr i l M.empty
rcModalAttr :: MonadWidget t m => Text -> [Class] -> AttributeMap -> m a -> m a
rcModalAttr i l a = rcDivAttr ([Modal] <> l) (inject a [("role", "dialog"), ("id", i)])

rcModalDialog :: MonadWidget t m => ModalSize -> [ModalOption] -> [Class] -> m a -> m a
rcModalDialog s o l = rcModalDialogAttr s o l M.empty
rcModalDialogAttr :: MonadWidget t m => ModalSize -> [ModalOption] -> [Class] -> AttributeMap -> m a -> m a
rcModalDialogAttr s o l a = rcDivAttr ([ModalDialog s o] <> l) (inject a [("role", "document")])

rcModalContent :: MonadWidget t m => [Class] -> m a -> m a
rcModalContent l = rcModalContentAttr l M.empty
rcModalContentAttr :: MonadWidget t m => [Class] -> AttributeMap -> m a -> m a
rcModalContentAttr l = rcDivAttr ([ModalContent] <> l)

rcModalHeader :: MonadWidget t m => [Class] -> m a -> m a
rcModalHeader l = rcModalHeaderAttr l M.empty
rcModalHeaderAttr :: MonadWidget t m => [Class] -> AttributeMap -> m a -> m a
rcModalHeaderAttr l = rcDivAttr ([ModalHeader] <> l)

rcModalBody :: MonadWidget t m => [Class] -> m a -> m a
rcModalBody l = rcModalBodyAttr l M.empty
rcModalBodyAttr :: MonadWidget t m => [Class] -> AttributeMap -> m a -> m a
rcModalBodyAttr l = rcDivAttr ([ModalBody] <> l)

rcModalFooter :: MonadWidget t m => [Class] -> m a -> m a
rcModalFooter l = rcModalFooterAttr l M.empty
rcModalFooterAttr :: MonadWidget t m => [Class] -> AttributeMap -> m a -> m a
rcModalFooterAttr l = rcDivAttr ([ModalFooter] <> l)

rcCloseModal :: MonadWidget t m => [Class] -> m (Event t ())
rcCloseModal l = rcCloseModalAttr l M.empty
rcCloseModalAttr :: MonadWidget t m => [Class] -> AttributeMap -> m (Event t ())
rcCloseModalAttr l a = do
    (e, _) <- elAttr' "button" (attr (inject a [("type", "button"),("data-dismiss", "modal"),("aria-label", "Close")]) ([CloseIcon] <> l)) $
      elAttr "span" (M.fromList [("aria-hidden","true")]) $ text "×"
    return $ domEvent Click e

rcForm :: MonadWidget t m => [Class] -> m a -> m a
rcForm l = rcFormAttr l M.empty
rcFormAttr :: MonadWidget t m => [Class] -> AttributeMap -> m a -> m a
rcFormAttr l a = elAttr "form" (attr (inject a [("role","form"),("novalidate","")]) l)

rcFormGroupRow :: MonadWidget t m => m a -> m a
rcFormGroupRow = rcDiv [FormGroup, Row]

-- | An compound widget for input text with library specific layout.
-- Arguments in order: type, id, label, placeholder, default value, wrapper classes, label classes, input classes
rcInputField :: MonadWidget t m => InputField -> Text -> Text -> Text -> Text -> [Class] -> [Class] -> [Class] -> m (TextInput t)
rcInputField t i l p d wc lc ic =
  rcDiv wc $ do
    rcLabel i l ([FormControlLabel] <> lc)
    rcRawInputField t i p d ic

-- | An compound widget for required input text with library specific layout.
-- Arguments in order: id, label, placeholder, default value, valid message, invalid message, wrapper classes, label classes, input classes, additional input attributes
-- To find input validation attributes refer to https://www.w3schools.com/html/html_form_input_types.asp
rcInputFieldV :: MonadWidget t m => InputField -> Text -> Text -> Text -> Text -> Text -> Text -> [Class] -> [Class] -> [Class] -> AttributeMap -> m (TextInput t)
rcInputFieldV t i l p d v iv wc lc ic a =
  rcDiv wc $ do
    rcLabel i l ([FormControlLabel] <> lc)
    r <- rcRawInputFieldAttr t i p d ic a
    rcDiv [ValidFeedback] $ text v
    rcDiv [InvalidFeedback] $ text iv
    return r

-- | Use this if you want a raw input field without library specific formatting.
rcRawInputField :: MonadWidget t m => InputField -> Text -> Text -> Text -> [Class] -> m (TextInput t)
rcRawInputField t i p d l = rcRawInputFieldAttr t i p d l M.empty
rcRawInputFieldAttr :: MonadWidget t m => InputField -> Text -> Text -> Text -> [Class] -> AttributeMap -> m (TextInput t)
rcRawInputFieldAttr t i p d l a =
  textInput $
    def & textInputConfig_inputType .~ fieldToText t
      & textInputConfig_attributes .~ constDyn (attr (inject a [("role","form"),("placeholder",p),("id",i)]) ([FormControl NoSize] <> l))
      & textInputConfig_initialValue .~ d

rcCheckbox :: MonadWidget t m => Bool -> Text -> Text -> [Class] -> m (Checkbox t)
rcCheckbox d i t l = rcCheckboxAttr d i t l M.empty
rcCheckboxAttr :: MonadWidget t m => Bool -> Text -> Text -> [Class] -> AttributeMap -> m (Checkbox t)
rcCheckboxAttr d i t l a =
  rcDiv ([FormCheck] <> l) $ do
    cb <- checkbox d $
      def & checkboxConfig_attributes .~ constDyn (attr (inject a [("id",i)]) [FormCheckInput])
    rcTagAttr "label" [FormCheckLabel] (M.fromList [("for",i)]) $ text t
    return cb

rcRadio :: MonadWidget t m => Bool -> Text -> Text -> Text -> [Class] -> m (FormElement t, Event t Bool)
rcRadio d i g t l = rcRadioAttr d i g t l M.empty

rcRadioAttr :: MonadWidget t m => Bool -> Text -> Text -> Text -> [Class] -> AttributeMap -> m (FormElement t, Event t Bool)
rcRadioAttr d i g t l a = do
  ie <- inputElement $ def
    & inputElementConfig_initialChecked .~ d
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
      mapKeysToAttributeName (attr (inject a [("id", i),("name",g),("type","radio")]) l)
  el "span" $ text t
  return (ie, _inputElement_checkedChange ie)

rcLabel :: MonadWidget t m => Text -> Text -> [Class] -> m ()
rcLabel f t l = rcLabelAttr f t l M.empty
rcLabelAttr :: MonadWidget t m => Text -> Text -> [Class] -> AttributeMap -> m ()
rcLabelAttr f t l a = rcTagAttr "label" l (inject a [("for", f)]) $ text t

rcAlert :: MonadWidget t m => ContextOption -> [Class] -> m a -> m (Event t ())
rcAlert c l = rcAlertAttr c l M.empty
rcAlertAttr :: MonadWidget t m => ContextOption -> [Class] -> AttributeMap -> m a -> m (Event t ())
rcAlertAttr c l a w =
  rcDivAttr ([Alert c, AlertDismissable, Show, Fade] <> l) (inject a [("role","alert")]) $ do
    (e, _) <- elAttr' "button" (attr (M.fromList [("type", "button"),("data-dismiss", "alert"),("aria-label", "Close")]) [CloseIcon]) $
      elAttr "span" (M.fromList [("aria-hidden","true")]) $ text "×"
    w
    return $ domEvent Click e
