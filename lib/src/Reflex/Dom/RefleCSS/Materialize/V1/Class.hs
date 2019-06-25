{-# LANGUAGE
    LambdaCase
#-}
{-|
  Module      : Reflex.Dom.RefleCSS.Materialize.V1.Class
  Description : Opinionated translation of ADT's to manage MaterializeCSS 1 Classes.
-}

module Reflex.Dom.RefleCSS.Materialize.V1.Class where

-- Base --
-- Third Party --
import qualified Data.Map as M
import Data.Text as T
import Reflex.Dom ( AttributeMap, ffor )
-- Local --
import Reflex.Dom.RefleCSS.Class
-- End Imports --

mkAttr :: [Class] -> AttributeMap
mkAttr = attr M.empty

attr :: AttributeMap -> [Class] -> AttributeMap
attr m l = case M.lookup "class" m of
  Just c -> M.insert "class" (c <> " " <> buildClasses l) m
  Nothing -> M.insert "class" (buildClasses l) m

buildClasses :: [Class] -> Text
buildClasses l = T.unwords (fmap buildClass l)

buildClass :: Class -> Text
buildClass = \case
  -- Layout
  Container -> "container" -- Translated
  ContainerFluid -> "container-fluid" -- Shimmed
  -- Grid
  Row -> "row" -- Translated
  Col NoSize o -> buildClass (Col ExtraSmall o) -- Translated
  Col s o -> "col" <> sizeToText " " s <> breakToText "" o -- Translated
  Offset NoSize o -> buildClass (Offset ExtraSmall o) -- Translated
  Offset s o -> "offset" <> sizeToText "-" s <> breakToText "" o -- Translated
  NoGutters -> "no-padding" -- Translated
  Order _ o -> "order" <> breakToText "-" o -- Shimmed
  OrderFirst -> "order-first" -- Shimmed
  OrderLast -> "order-last" -- Shimmed
  -- Visibility
  Visible -> "visible" -- Shimmed
  Invisible -> "hide" -- Translated
  -- Justify
  JustifyContent _ o -> "justify-content" <> justifyToText "-" o -- Shimmed
  Align o -> "align" <> verticalAlignToText "-" o -- Shimmed
  AlignItems _ o -> "align-items" <> alignToText "-" o -- Shimmed
  AlignSelf _ o -> "align-self" <> alignToText "-" o -- Shimmed
  AlignContent _ o -> "align-content" <> alignToText "-" o -- Shimmed
  -- Sizing
  W s -> "w" <> percentToText "-" s -- Shimmed
  H s -> "h" <> percentToText "-" s -- Shimmed
  MW s -> "mw" <> percentToText "-" s -- Shimmed
  MH s -> "mh" <> percentToText "-" s -- Shimmed
  VH s -> "vh" <> percentToText "-" s -- Shimmed
  VW s -> "vw" <> percentToText "-" s -- Shimmed
  MinVH s -> "min-vh" <> percentToText "-" s -- Shimmed
  MinVW s -> "min-vw" <> percentToText "-" s -- Shimmed
  M s _ g -> "m" <> sideToText s <> gapToText "-" g -- Shimmed
  P s _ g -> "p" <> sideToText s <> gapToText "-" g -- Shimmed
  -- Image
  ImgFluid -> "materialboxed" -- Translated
  ImgThumbnail -> "materialboxed" -- Translated
  -- Table
  Table l -> T.unwords (fmap tableToText l) -- Translated
  TableResponsive _ -> "table-responsive" -- Translated
  THeadDark -> "thead-dark" -- No Effect
  THeadLight -> "thead-light" -- No Effect
  TableContext c -> contextToText "table-" c -- No Effect
  -- Figure
  Figure -> "figure" -- No Effect
  FigureImg -> "figure-img" -- No Effect
  FigureCaption -> "figure-caption" -- No Effect
  -- Display (Typography)
  Display1 -> "display-1" -- No Effect
  Display2 -> "display-2" -- No Effect
  Display3 -> "display-3" -- No Effect
  Display4 -> "display-4" -- No Effect
  -- Lead
  Lead -> "lead" -- No Effect
  -- Blockquote
  Blockquote -> "blockquote" -- No Effect
  BlockquoteFooter -> "blockquote-footer" -- No Effect
  -- Text
  TextAlign _ o -> textAlignToText o -- Translated
  TextWrap -> "text-wrap" -- Shimmed
  TextNowrap -> "text-nowrap" -- Shimmed
  TextTruncate -> "text-truncate" -- Shimmed
  TextBreak -> "text-break" -- Shimmed
  TextMuted -> "text-muted" -- No Effect
  TextUppercase -> "text-uppercase" -- Shimmed
  TextLowercase -> "text-lowercase" -- Shimmed
  TextCapitalize -> "text-capitalize" -- Shimmed
  TextMonospace -> "text-monospace" -- Shimmed
  TextReset -> "text-reset" -- Shimmed
  TextDecorationNone -> "text-decoration-none" -- Shimmed
  TextContext PrimaryContext -> "" -- Name Clash text-primary from TimePicker
  TextContext c -> contextToText "text-" c -- No Effect
  -- Font
  FontItalic -> "font-italic"
  FontWeight o -> "font-weight" <> fontWeightToText "-" o
  -- Lists
  ListUnstyled -> "list-unstyled"
  ListInline -> "list-inline"
  ListInlineItem -> "list-inline-item"
  -- Alert
  Alert c -> "alert " <> contextToText "alert-" c
  AlertLink -> "alert-link"
  AlertHeading -> "alert-heading"
  AlertDismissable -> "alert-dismissable"
  -- Badge
  Badge c -> "badge " <> contextToText "badge-" c
  BadgePill c -> "badge badge-pill " <> contextToText "badge-" c
  -- Breadcrumb
  Breadcrumb -> "breadcrumb"
  BreadcrumbItem -> "breadcrumb-item"
  -- Button
  Btn LinkButton _ z -> "btn-link" <> " " <> buttonSizeToText z -- Shimmed
  Btn _ c z -> "btn" <> " " <> contextToText "btn-" c <> " " <> buttonSizeToText z -- Translated
  -- Button Group
  BtnToolbar -> "btn-toolbar" -- No Effect
  BtnGroup z -> "btn-group" <> " " <> buttonGroupSizeToText z -- Shimmed
  BtnGroupVertical -> "btn-group-vertical" -- Shimmed
  -- Card
  Card -> "card"
  CardBody -> "card-content"
  CardTitle -> "card-title"
  CardHeader -> "card-header" -- No Effect
  CardFooter -> "card-action"
  CardSubtitle -> "card-subtitle"
  CardText -> "card-text"
  CardImgTop -> "card-img-top"
  CardLink -> "card-link"
  CardDeck -> "card-deck"
  CardColumns -> "card-columns"
  CardGroup -> "card-group"
  -- Carousel
  Carousel -> "carousel"
  CarouselInner -> "carousel-inner"
  CarouselItem -> "carousel-item"
  CarouselCaption -> "carousel-caption"
  CarouselFade -> "carousel-fade"
  CarouselIndicators -> "carousel-indicators"
  CarouselControlPrev -> "carousel-control-prev"
  CarouselControlNext -> "carousel-control-next"
  CarouselControlPrevIcon -> "carousel-control-prev-icon"
  CarouselControlNextIcon -> "carousel-control-next-icon"
  -- Collapse
  Collapse -> "collapse"
  MultiCollapse -> "collapse multi-collapse"
  Collapsed -> "collapsed"
  Accordion -> "accordion"
  -- Dropdown
  Dropdown -> "dropdown"
  DropUp -> "dropup"
  DropRight -> "dropright"
  DropLeft -> "dropleft"
  DropdownToggle -> "dropdown-trigger" -- Shimmed
  DropdownToggleSplit -> "dropdown-trigger" -- Shimmed
  DropdownMenu -> "dropdown-content" -- Translated
  DropdownMenuRight _ -> "dropdown-content" -- Translated
  DropdownMenuLeft _ -> "dropdown-content" -- Translated
  DropdownHeader -> "dropdown-header" -- Shimmed
  DropdownItem -> "dropdown-item"
  DropdownDivider -> "divider" -- Translated
  -- Forms
  FormInline -> "form-inline"
  FormGroup -> "form-group"
  FormControl NoSize -> "form-control"
  FormControl s -> "form-control form-control" <> sizeToText "-" s
  FormControlPlaintext -> "form-control-plaintext"
  FormControlLabel -> "form-control-label"
  FormText -> "form-text"
  FormControlFile -> "form-control-file"
  FormControlRange -> "form-control-range"
  FormCheck -> "form-check"
  FormCheckInline -> "form-check form-check-inline"
  FormCheckInput -> "form-check-input"
  FormCheckLabel -> "form-check-label"
  FormRow -> "form-row"
  ColFormLabel s -> "col-form-label" <> sizeToText "-" s
  FormSelect -> "form-select"
  -- Tooltips
  ValidFeedback -> "valid-feedback"
  InvalidFeedback -> "invalid-feedback"
  ValidTooltip -> "valid-tooltip"
  InvalidTooltip -> "invalid-tooltip"
  WasValidated -> "was-validated"
  -- Input Group
  InputGroup NoSize -> "input-group"
  InputGroup s -> "input-group input-group" <> sizeToText "-" s
  InputGroupPrepend -> "input-group-prepend"
  InputGroupAppend -> "input-group-append"
  InputGroupText -> "input-group-text"
  -- Jumbotron
  Jumbotron -> "jumbotron" -- No Effect
  JumbotronFluid -> "jumbotron jumbotron-fluid" -- No Effect
  -- List Group
  ListGroup -> "collection"
  ListGroupFlush -> "collection"
  ListGroupHorizontal _ -> "collection"
  ListGroupItem _ -> "collection-item"
  ListGroupItemAction -> ""
  -- Media
  Media -> "media"
  MediaBody -> "media-body"
  -- Modal
  Modal -> "modal"
  ModalDialog s l -> "modal-dialog " <> modalSizeToText s <> " " <> modalOptionsToText l -- No Effect
  ModalContent -> "modal-content"
  ModalHeader -> "modal-header"
  ModalTitle -> "modal-title float-left"
  ModalBody -> "modal-body clear-fix" -- Shimmed
  ModalFooter -> "modal-footer"
  -- Nav
  Nav l -> "nav " <> navOptionsToText l -- Translated
  NavItem -> "collection-item" -- Translated
  NavLink -> "nav-link"
  -- Navbar
  Navbar l -> "navbar " <> navbarOptionsToText l
  NavbarBrand -> "brand-logo"
  NavbarToggler -> "sidenav-trigger"
  NavbarTogglerIcon -> "material-icons"
  NavbarText -> "navbar-text"
  NavbarCollapse -> "sidenav"
  NavbarNav -> "navbar-nav"
  -- Tabs
  TabContent -> "tab-content"
  TabPane -> "tab-pane"
  -- Pagination
  Pagination s -> paginationSizeToText s
  PageItem -> "page-item"
  PageLink -> "page-link"
  -- Progress
  Progress -> "progress"
  ProgressBar -> "progress-bar"
  ProgressBarStriped -> "progress-bar-striped"
  ProgressBarAnimated -> "progress-bar-animated"
  -- Spinner
  SpinnerBorder -> "spinner-border"
  SpinnerGrow -> "spinner-grow"
  SpinnerBorderSM -> "spinner-border spinner-border-sm"
  SpinnerGrowSM -> "spinner-grow spinner-grow-sm"
  -- Toast
  Toast -> "toast"
  ToastHeader -> "toast-header"
  ToastBody -> "toast-body"
  -- Borders
  Border o -> borderToText o
  BorderContext c -> "border" <> contextToText "-" c
  -- Display (Visibility)
  Display s o -> "d" <> displaySizeToText "-" s <> displayToText "-" o -- Shimmed
  -- Embed
  Embed21X9 -> "embed-responsive embed-responsive-21by9" -- No Effect
  Embed16X9 -> "embed-responsive embed-responsive-16by9" -- No Effect
  Embed4X3 -> "embed-responsive embed-responsive-4by3" -- No Effect
  Embed1X1 -> "embed-responsive embed-responsive-1by19" -- No Effect
  EmbedItem -> "embed-responsive-item" -- No Effect
  -- Float
  FloatLeft _ -> "float-left" -- Shimmed
  FloatRight _ -> "float-right" -- Shimmed
  FloatNone _ -> "float-none" -- Shimmed
  -- Position
  Position o -> positionToText "-" o
  -- Shadows
  Shadow -> "shadow"
  ShadowNone -> "shadow-none"
  ShadowSM -> "shadow-sm"
  ShadowLG -> "shadow-lg"
  -- Util
  OverflowAuto -> "overflow-auto"
  OverflowHidden -> "overflow-hidden"
  TextHide -> "text-hide"
  CloseIcon -> "close"
  ClearFix -> "clear-fix" -- Shimmed
  Active -> "active"
  Disabled -> "disabled"
  FixedTop -> "fixed-top"
  FixedBottom -> "fixed-bottom"
  StickyTop -> "sticky-top"
  Fade -> "fade"
  Show -> "show"
  Slide -> "slide"
  SROnly -> "sr-only" -- Shimmed
  SROnlyFocusable -> "sr-only-focusable" -- Shimmed
  FlexNowrap -> "flex-nowrap"
  Context c -> contextToText "" c -- No Effect
  Bg c -> contextToText "" c -- No Effect
  BgGradient c -> contextToText "" c -- No Effect
  StretchedLink -> "stretched-link"
  FlexColumn -> "flex-column" -- Shimmed

  Custom l -> T.unwords l

contextToText :: Text -> ContextOption -> Text
contextToText p = \case
  NoContext -> ""
  ActiveContext -> p <> "active"
  PrimaryContext -> p <> "primary"
  SecondaryContext -> p <> "secondary"
  SuccessContext -> p <> "success"
  DangerContext -> p <> "danger"
  WarningContext -> p <> "warning"
  InfoContext -> p <> "info"
  LightContext -> p <> "light"
  WhiteContext -> p <> "white"
  DarkContext -> p <> "dark"
  BlackContext -> p <> "black"
  TransparentContext -> p <> "transparent"
  BodyContext -> p <> "body"

verticalAlignToText :: Text -> VerticalAlignOption -> Text
verticalAlignToText p = \case
  VerticalBaseline -> p <> "baseline"
  VerticalTop -> p <> "top"
  VerticalMiddle -> p <> "middle"
  VerticalBottom -> p <> "bottom"
  TextToVerticalTop -> p <> "text-top"
  TextToVerticalBottom -> p <> "text-bottom"

fontWeightToText :: Text -> FontWeightOption -> Text
fontWeightToText p = \case
  BoldText -> p <> "bold"
  BolderText -> p <> "bolder"
  NormalText -> p <> "normal"
  LightText -> p <> "light"
  DarkText -> p <> "lighter"

textAlignToText :: TextAlignOption -> Text
textAlignToText = \case
  TextLeft -> "left-align"
  TextCenter -> "center-align"
  TextRight -> "right-align"

positionToText :: Text -> PositionOption -> Text
positionToText p = \case
  StaticPosition -> p <> "static"
  RelativePosition -> p <> "relative"
  AbsolutePosition -> p <> "absolute"
  FixedPosition -> p <> "fixed"
  SitckyPosition -> p <> "sticky"

floatSizeToText :: Text -> FloatSize -> Text
floatSizeToText p = \case
  FloatOnAll -> ""
  FloatOnSmall -> p <> "sm"
  FloatOnMedium -> p <> "md"
  FloatOnLarge -> p <> "lg"
  FloatOnExtraLarge -> p <> "xl"

displaySizeToText :: Text -> DisplaySize -> Text
displaySizeToText p = \case
  DisplayDefault -> ""
  DisplayOnSmall -> p <> "sm"
  DisplayOnMedium -> p <> "md"
  DisplayOnLarge -> p <> "lg"
  DisplayOnExtraLarge -> p <> "xl"
  DisplayOnPrint -> p <> "print"

displayToText :: Text -> DisplayOption -> Text
displayToText p = \case
  DisplayNone -> p <> "none"
  DisplayInline -> p <> "inline"
  DisplayInlineBlock -> p <> "inline-block"
  DisplayBlock -> p <> "block"
  DisplayTable -> p <> "table"
  DisplayCell -> p <> "table-cell"
  DisplayRow -> p <> "table-row"
  DisplayFlex -> p <> "flex"
  DisplayInlineFlex -> p <> "inline-flex"

borderToText :: BorderOption -> Text
borderToText = \case
  AllBorders -> "border"
  TopBorder -> "border-top"
  RightBorder -> "border-right"
  BottomBorder -> "border-bottom"
  LeftBorder -> "border-left"
  NoBorder -> "border-0"
  NoTopBorder -> "border-top-0"
  NoRightBorder -> "border-right-0"
  NoBottomBorder -> "border-bottom-0"
  NoLeftBorder -> "border-left-0"
  Rounded -> "rounded"
  TopRounded -> "rounded-top"
  RightRounded -> "rounded-right"
  BottomRounded -> "rounded-bottom"
  LeftRounded -> "rounded-left"
  CircleRounded -> "rounded-circle"
  PillRounded -> "rounded-pill"
  NoRounded -> "rounded-0"
  RoundedSmall -> "rounded-sm"
  RoundedLarge -> "rounded-lg"

paginationSizeToText :: PaginationSize -> Text
paginationSizeToText = \case
  Paginate -> "pagination"
  PaginateSmall -> "pagination pagination-sm"
  PaginateLarge -> "pagination pagination-lg"

navbarOptionsToText :: [NavbarOption] -> Text
navbarOptionsToText l = T.unwords . ffor l $ \case
  ExpandSmall -> "navbar-expand-sm"
  ExpandMedium -> "navbar-expand-md"
  ExpandLarge -> "navbar-expand-lg"
  ExpandExtraLarge -> "navbar-expand-xl"
  LightNavbar -> "navbar-light"
  DarkNavbar -> "navbar-dark"

navOptionsToText :: [NavOption] -> Text
navOptionsToText l = T.unwords . ffor l $ \case
  NavTabs -> "tabs" -- Translated
  NavPills -> "collection" -- Translated
  NavFill -> "nav-fill" -- No Effect
  NavJustified -> "nav-justified" -- No Effect

modalSizeToText :: ModalSize -> Text
modalSizeToText = \case
  MediumModal -> ""
  SmallModal -> "modal-sm"
  LargeModal -> "modal-lg"
  ExtraLargeModal -> "modal-xl"

modalOptionsToText :: [ModalOption] -> Text
modalOptionsToText l = T.unwords . ffor l $ \case
  ModalCenter -> "modal-dialog-centered"
  ModalScroll -> "modal-dialog-scrollable"

buttonSizeToText :: ButtonSizeOption -> Text
buttonSizeToText = \case
  MediumButton -> "" -- Translated
  SmallButton -> "btn-small" -- Translated
  LargeButton -> "btn-large" -- Translated
  BlockButton -> "btn-block" -- Shimmed

buttonGroupSizeToText :: ButtonGroupSizeOption -> Text
buttonGroupSizeToText = \case
  ButtonGroupMedium -> ""
  ButtonGroupSmall -> "btn-group-sm"
  ButtonGroupLarge -> "btn-group-lg"

tableToText :: TableOption -> Text
tableToText = \case
  TableDark -> "table-dark" -- No Effect
  TableStriped -> "striped" --
  TableBordered -> "table-bordered" -- No Effect
  TableBorderless -> "table-borderless" -- No Effect
  TableHover -> "highlight" --
  TableSmall -> "table-sm" -- No Effect

alignToText :: Text -> AlignOption -> Text
alignToText p = \case
  AlignStart -> p <> "start"
  AlignEnd -> p <> "end"
  AlignCenter -> p <> "center"
  AlignBaseline -> p <> "baseline"
  AlignStretch -> p <> "stretch"

justifyToText :: Text -> JusitfyOption -> Text
justifyToText p = \case
  JusitfyStart -> p <> "start"
  JusitfyEnd -> p <> "end"
  JusitfyCenter -> p <> "center"
  JusitfyBetween -> p <> "between"
  JusitfyAround -> p <> "around"

gapToText :: Text -> GapOption -> Text
gapToText p = \case
  Gap0    -> p <> "0"
  Gap1    -> p <> "1"
  Gap2    -> p <> "2"
  Gap3    -> p <> "3"
  Gap4    -> p <> "4"
  Gap5    -> p <> "5"
  GapNeg1    -> p <> "n1"
  GapNeg2    -> p <> "n2"
  GapNeg3    -> p <> "n3"
  GapNeg4    -> p <> "n4"
  GapNeg5    -> p <> "n5"
  GapAuto    -> p <> "auto"

sideToText :: SideOption -> Text
sideToText = \case
  TopSide -> "t"
  BottomSide -> "b"
  LeftSide -> "l"
  RightSide -> "r"
  XSides -> "x"
  YSides -> "y"
  AllSides -> ""

percentToText :: Text -> PercentOption -> Text
percentToText p = \case
  Percent100  -> p <> "100"
  Percent75   -> p <> "75"
  Percent50   -> p <> "50"
  Percent25   -> p <> "25"
  PercentAuto -> p <> "auto"

sizeToText :: Text -> SizeOption -> Text
sizeToText p = \case
  NoSize -> ""
  ExtraSmall -> p <> "s"
  Small -> p <> "m"
  Medium -> p <> "l"
  Large -> p <> "xl"
  ExtraLarge -> p <> "xl"

breakToText :: Text -> BreakOption -> Text
breakToText p = \case
  NoBreak   -> ""
  Break0   -> p <> "0"
  Break1   -> p <> "1"
  Break2   -> p <> "2"
  Break3   -> p <> "3"
  Break4   -> p <> "4"
  Break5   -> p <> "5"
  Break6   -> p <> "6"
  Break7   -> p <> "7"
  Break8   -> p <> "8"
  Break9   -> p <> "9"
  Break10  -> p <> "10"
  Break11  -> p <> "11"
  Break12  -> p <> "12"
  BreakAuto   -> p <> "auto"
