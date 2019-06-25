{-# LANGUAGE
    LambdaCase
#-}
{-|
  Module      : Reflex.Dom.RefleCSS.Bootstrap.V4.Class
  Description : ADTs translation functions for managing Bootstrap 4 CSS Classes.
-}

module Reflex.Dom.RefleCSS.Bootstrap.V4.Class where

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
  Container -> "container"
  ContainerFluid -> "container-fluid"
  -- Grid
  Row -> "row"
  Col s o -> "col" <> sizeToText "-" s <> breakToText "-" o
  Offset s o -> "offset" <> sizeToText "-" s <> breakToText "-" o
  NoGutters -> "no-gutters"
  Order s o -> "order" <> sizeToText "-" s <> breakToText "-"  o
  OrderFirst -> "order-first"
  OrderLast -> "order-last"
  -- Visibility
  Visible -> "visible"
  Invisible -> "invisible"
  -- Justify
  JustifyContent s o -> "justify-content" <> sizeToText "-" s <> justifyToText "-" o
  Align o -> "align" <> verticalAlignToText "-" o
  AlignItems s o -> "align-items" <> sizeToText "-" s <> alignToText "-" o
  AlignSelf s o -> "align-self" <> sizeToText "-" s <> alignToText "-" o
  AlignContent s o -> "align-content" <> sizeToText "-" s <> alignToText "-" o
  -- Sizing
  W s -> "w" <> percentToText "-" s
  H s -> "h" <> percentToText "-" s
  MW s -> "mw" <> percentToText "-" s
  MH s -> "mh" <> percentToText "-" s
  VH s -> "vh" <> percentToText "-" s
  VW s -> "vw" <> percentToText "-" s
  MinVH s -> "min-vh" <> percentToText "-" s
  MinVW s -> "min-vw" <> percentToText "-" s
  M s ExtraSmall g -> "m" <> sideToText s <> gapToText "-" g
  P s ExtraSmall g -> "p" <> sideToText s <> gapToText "-" g
  M s z g -> "m" <> sideToText s <> sizeToText "-" z <> gapToText "-" g
  P s z g -> "p" <> sideToText s <> sizeToText "-" z <> gapToText "-" g
  -- Image
  ImgFluid -> "img-fluid"
  ImgThumbnail -> "img-thumbnail"
  -- Table
  Table l -> T.unwords (["table"] <> fmap tableToText l)
  TableResponsive z -> "table-responsive" <> sizeToText "-" z
  THeadDark -> "thead-dark"
  THeadLight -> "thead-light"
  TableContext c -> contextToText "table-" c
  -- Figure
  Figure -> "figure"
  FigureImg -> "figure-img"
  FigureCaption -> "figure-caption"
  -- Display (Typography)
  Display1 -> "display-1"
  Display2 -> "display-2"
  Display3 -> "display-3"
  Display4 -> "display-4"
  -- Lead
  Lead -> "lead"
  -- Blockquote
  Blockquote -> "blockquote"
  BlockquoteFooter -> "blockquote-footer"
  -- Text
  TextAlign s o -> "text" <> floatSizeToText "-" s <> textAlignToText "-" o
  TextWrap -> "text-wrap"
  TextNowrap -> "text-nowrap"
  TextTruncate -> "text-truncate"
  TextBreak -> "text-break"
  TextMuted -> "text-muted"
  TextUppercase -> "text-uppercase"
  TextLowercase -> "text-lowercase"
  TextCapitalize -> "text-capitalize"
  TextMonospace -> "text-monospace"
  TextReset -> "text-reset"
  TextDecorationNone -> "text-decoration-none"
  TextContext c -> contextToText "text-" c
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
  Btn NormalButton c z -> "btn" <> " " <> contextToText "btn-" c <> " " <> buttonSizeToText z
  Btn OutlineButton c z -> "btn" <> " " <> contextToText "btn-outline-" c <> " " <> buttonSizeToText z
  Btn LinkButton _ z -> "btn btn-link" <> " " <> buttonSizeToText z
  -- Button Group
  BtnToolbar -> "btn-toolbar"
  BtnGroup z -> "btn-group" <> " " <> buttonGroupSizeToText z
  BtnGroupVertical -> "btn-group-vertical"
  -- Card
  Card -> "card"
  CardBody -> "card-body"
  CardTitle -> "card-title"
  CardHeader -> "card-header"
  CardFooter -> "card-footer"
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
  DropdownToggle -> "dropdown-toggle"
  DropdownToggleSplit -> "dropdown-toggle dropdown-toggle-split"
  DropdownMenu -> "dropdown-menu"
  DropdownMenuRight ExtraSmall -> "dropdown-menu dropdown-menu-right"
  DropdownMenuLeft ExtraSmall -> "dropdown-menu dropdown-menu-left"
  DropdownMenuRight NoSize -> "dropdown-menu dropdown-menu-right"
  DropdownMenuLeft NoSize -> "dropdown-menu dropdown-menu-left"
  DropdownMenuRight s -> "dropdown-menu " <> sizeToText "dropdown-menu-" s <> "-right"
  DropdownMenuLeft s -> "dropdown-menu " <> sizeToText "dropdown-menu-" s <> "-left"
  DropdownHeader -> "dropdown-header"
  DropdownItem -> "dropdown-item"
  DropdownDivider -> "dropdown-divider"
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
  Jumbotron -> "jumbotron"
  JumbotronFluid -> "jumbotron jumbotron-fluid"
  -- List Group
  ListGroup -> "list-group"
  ListGroupFlush -> "list-group list-group-flush"
  ListGroupHorizontal s -> "list-group list-group-horizontal" <> sizeToText "-" s
  ListGroupItem NoContext -> "list-group-item"
  ListGroupItem c -> "list-group-item list-group-item" <> contextToText "-" c
  ListGroupItemAction -> "list-group-item-action"
  -- Media
  Media -> "media"
  MediaBody -> "media-body"
  -- Modal
  Modal -> "modal"
  ModalDialog MediumModal l -> "modal-dialog " <> modalOptionsToText l
  ModalDialog s l -> "modal-dialog " <> modalSizeToText s <> " " <> modalOptionsToText l
  ModalContent -> "modal-content"
  ModalHeader -> "modal-header"
  ModalTitle -> "modal-title"
  ModalBody -> "modal-body"
  ModalFooter -> "modal-footer"
  -- Nav
  Nav l -> "nav " <> navOptionsToText l
  NavItem -> "nav-item"
  NavLink -> "nav-link"
  -- Navbar
  Navbar l -> "navbar " <> navbarOptionsToText l
  NavbarBrand -> "navbar-brand"
  NavbarToggler -> "navbar-toggler"
  NavbarTogglerIcon -> "navbar-toggler-icon"
  NavbarText -> "navbar-text"
  NavbarCollapse -> "navbar-collapse"
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
  Display s o -> "d" <> displaySizeToText "-" s <> displayToText "-" o
  -- Embed
  Embed21X9 -> "embed-responsive embed-responsive-21by9"
  Embed16X9 -> "embed-responsive embed-responsive-16by9"
  Embed4X3 -> "embed-responsive embed-responsive-4by3"
  Embed1X1 -> "embed-responsive embed-responsive-1by19"
  EmbedItem -> "embed-responsive-item"
  -- Float
  FloatLeft s -> "float" <> floatSizeToText "-" s <> "-left"
  FloatRight s -> "float" <> floatSizeToText "-" s <> "-right"
  FloatNone s -> "float" <> floatSizeToText "-" s <> "-none"
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
  ClearFix -> "clearfix"
  Active -> "active"
  Disabled -> "disabled"
  FixedTop -> "fixed-top"
  FixedBottom -> "fixed-bottom"
  StickyTop -> "sticky-top"
  Fade -> "fade"
  Show -> "show"
  Slide -> "slide"
  SROnly -> "sr-only"
  SROnlyFocusable -> "sr-only-focusable"
  FlexNowrap -> "flex-nowrap"
  Context c -> contextToText "" c
  Bg c -> contextToText "bg-" c
  BgGradient c -> contextToText "bg-gradient-" c
  StretchedLink -> "stretched-link"
  FlexColumn -> "flex-column"

  Custom l -> T.unwords l

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

textAlignToText :: Text -> TextAlignOption -> Text
textAlignToText p = \case
  TextLeft -> p <> "left"
  TextCenter -> p <> "center"
  TextRight -> p <> "right"

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
  NavTabs -> "nav-tabs"
  NavPills -> "nav-pills"
  NavFill -> "nav-fill"
  NavJustified -> "nav-justified"

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
  MediumButton -> ""
  SmallButton -> "btn-sm"
  LargeButton -> "btn-lg"
  BlockButton -> "btn-block"

buttonGroupSizeToText :: ButtonGroupSizeOption -> Text
buttonGroupSizeToText = \case
  ButtonGroupMedium -> ""
  ButtonGroupSmall -> "btn-group-sm"
  ButtonGroupLarge -> "btn-group-lg"

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

tableToText :: TableOption -> Text
tableToText = \case
  TableDark -> "table-dark"
  TableStriped -> "table-striped"
  TableBordered -> "table-bordered"
  TableBorderless -> "table-borderless"
  TableHover -> "table-hover"
  TableSmall -> "table-sm"

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
  PercentAuto    -> p <> "auto"

sizeToText :: Text -> SizeOption -> Text
sizeToText p = \case
  NoSize -> ""
  ExtraSmall -> p <> "xs"
  Small -> p <> "sm"
  Medium -> p <> "md"
  Large -> p <> "lg"
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
