{-|
  Module      : Reflex.Dom.RefleCSS.Materialize.V1.Dom
  Description : Helpers for building Bootstrap 4 widgets.
-}

module Reflex.Dom.RefleCSS.Materialize.V1.Dom where

-- Base --
-- Third Party --
import qualified Data.Map as M
import Data.Text hiding ( foldr )
import Reflex.Dom
-- Local --
import Reflex.Dom.RefleCSS.Class
import Reflex.Dom.RefleCSS.Materialize.V1.Class
-- End Imports --

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

rcNav :: MonadWidget t m => [NavOption] -> [Class] -> m a -> m a
rcNav n l = rcNavAttr n l M.empty
rcNavAttr :: MonadWidget t m => [NavOption] -> [Class] -> AttributeMap -> m a -> m a
rcNavAttr n l = rcTagAttr "ul" ([Nav n] <> l)

rcNavbar :: MonadWidget t m => [NavbarOption] -> [Class] -> m a -> m a
rcNavbar n l = rcNavbarAttr n l M.empty
rcNavbarAttr :: MonadWidget t m => [NavbarOption] -> [Class] -> AttributeMap -> m a -> m a
rcNavbarAttr n l = rcDivAttr ([Navbar n] <> l)

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
    elAttr' "button" (attr (inject a [("data-toggle","collapse"),("data-target", t),("aria-expanded", "false"), ("aria-label", "Toggle navigation")]) ([NavbarToggler] <> l)) $
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

rcNavTab :: MonadWidget t m => Text -> [Class] -> m a -> m (Event t ())
rcNavTab t l = rcNavTabAttr t l M.empty
rcNavTabAttr :: MonadWidget t m => Text -> [Class] -> AttributeMap -> m a -> m (Event t ())
rcNavTabAttr t l a w = do
  (e, _) <- elAttr' "a" (attr (inject a [("href", t),("data-toggle", "tab")]) ([NavLink] <> l)) w
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

rcBtn :: MonadWidget t m => ButtonType -> ContextOption -> ButtonSizeOption -> [Class] -> m a -> m (Event t ())
rcBtn t c s l = rcBtnAttr t c s l M.empty
rcBtnAttr :: MonadWidget t m => ButtonType -> ContextOption -> ButtonSizeOption -> [Class] -> AttributeMap -> m a -> m (Event t ())
rcBtnAttr t c s l a w = do
  (e, _) <- elAttr' "button" (attr a ([Btn t c s] <> l)) w
  return $ domEvent Click e

rcLinkBtn :: MonadWidget t m => ButtonType -> ContextOption -> ButtonSizeOption -> [Class] -> m a -> m (Event t ())
rcLinkBtn t c s l = rcLinkBtnAttr t c s l M.empty
rcLinkBtnAttr :: MonadWidget t m => ButtonType -> ContextOption -> ButtonSizeOption -> [Class] -> AttributeMap -> m a -> m (Event t ())
rcLinkBtnAttr t c s l a w = do
  (e, _) <- elAttr' "a" (attr a ([Btn t c s] <> l)) w
  return $ domEvent Click e

rcToggleSplit :: MonadWidget t m => ButtonType -> ContextOption -> ButtonSizeOption -> [Class] -> m a -> m (Event t ())
rcToggleSplit b c s l = rcToggleSplitAttr b c s l M.empty
rcToggleSplitAttr :: MonadWidget t m => ButtonType -> ContextOption -> ButtonSizeOption -> [Class] -> AttributeMap -> m a -> m (Event t ())
rcToggleSplitAttr b c s l a =
  rcBtnAttr b c s ([DropdownToggleSplit] <> l) (inject a [("data-toggle", "dropdown"),("aria-haspopup","true"),("aria-expanded","false")])

rcToggle :: MonadWidget t m => ButtonType -> ContextOption -> ButtonSizeOption -> [Class] -> m a -> m (Event t ())
rcToggle b c s l = rcToggleAttr b c s l M.empty
rcToggleAttr :: MonadWidget t m => ButtonType -> ContextOption -> ButtonSizeOption -> [Class] -> AttributeMap -> m a -> m (Event t ())
rcToggleAttr b c s l a =
  rcBtnAttr b c s ([DropdownToggle] <> l) (inject a [("data-toggle", "dropdown"),("aria-haspopup","true"),("aria-expanded","false")])

rcDropdownMenu :: MonadWidget t m => [Class] -> m a -> m a
rcDropdownMenu l = rcDropdownMenuAttr l M.empty
rcDropdownMenuAttr :: MonadWidget t m => [Class] -> AttributeMap -> m a -> m a
rcDropdownMenuAttr l = rcDivAttr ([DropdownMenu] <> l)

rcDropdownMenuRight :: MonadWidget t m => SizeOption -> [Class] -> m a -> m a
rcDropdownMenuRight s l = rcDropdownMenuRightAttr s l M.empty
rcDropdownMenuRightAttr :: MonadWidget t m => SizeOption -> [Class] -> AttributeMap -> m a -> m a
rcDropdownMenuRightAttr s l = rcDivAttr ([DropdownMenuRight s] <> l)

rcDropdownMenuLeft :: MonadWidget t m => SizeOption -> [Class] -> m a -> m a
rcDropdownMenuLeft s l = rcDropdownMenuLeftAttr s l M.empty
rcDropdownMenuLeftAttr :: MonadWidget t m => SizeOption -> [Class] -> AttributeMap -> m a -> m a
rcDropdownMenuLeftAttr s l = rcDivAttr ([DropdownMenuLeft s] <> l)

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
    (e, _) <- elAttr' "button" (attr (inject a [("data-dismiss", "modal"),("aria-label", "Close")]) ([CloseIcon] <> l)) $
      elAttr "span" (M.fromList [("aria-hidden","true")]) $ text "×"
    return $ domEvent Click e

rcForm :: MonadWidget t m => [Class] -> m a -> m a
rcForm l = rcFormAttr l M.empty
rcFormAttr :: MonadWidget t m => [Class] -> AttributeMap -> m a -> m a
rcFormAttr l a = elAttr "form" (attr (inject a [("role","form")]) l)

rcTextInput :: MonadWidget t m => Text -> Text -> [Class] -> m (TextInput t)
rcTextInput p d l = rcTextInputAttr p d l M.empty
rcTextInputAttr :: MonadWidget t m => Text -> Text -> [Class] -> AttributeMap -> m (TextInput t)
rcTextInputAttr p d l a =
  textInput $
    def & textInputConfig_inputType .~ "text"
      & textInputConfig_attributes .~ constDyn (attr (inject a [("role","form"),("placeholder",p)]) ([FormControl NoSize] <> l))
      & textInputConfig_initialValue .~ d

rcCloseAlert :: MonadWidget t m => [Class] -> m (Event t ())
rcCloseAlert l = rcCloseAlertAttr l M.empty
rcCloseAlertAttr :: MonadWidget t m => [Class] -> AttributeMap -> m (Event t ())
rcCloseAlertAttr l a = do
    (e, _) <- elAttr' "button" (attr (inject a [("data-dismiss", "alert"),("aria-label", "Close")]) ([CloseIcon] <> l)) $
      elAttr "span" (M.fromList [("aria-hidden","true")]) $ text "×"
    return $ domEvent Click e

-- Anything in the existing attribute map will be overridden by vaules in the list.
inject :: AttributeMap -> [(Text, Text)] -> AttributeMap
inject = foldr (\(k, v) a -> M.insert k v a)
