{-# LANGUAGE
    LambdaCase
#-}
{-|
  Module      : Reflex.Dom.Bootstrap.V4.Class
  Description : Extensive ADTs for managing CSS Framework Classes.
-}

module Reflex.Dom.RefleCSS.Class where

-- Base --
-- Third Party --
import Data.Text as T
-- Local --
-- End Imports --

data Class =
    -- Layout
    Container
  | ContainerFluid
    -- Grid
  | Row
  | Col SizeOption BreakOption
  | Offset SizeOption BreakOption
  | NoGutters
  | Order SizeOption BreakOption
  | OrderFirst
  | OrderLast
    -- Visibility
  | Visible
  | Invisible
    -- Justify
  | JustifyContent SizeOption JusitfyOption
  | Align VerticalAlignOption
  | AlignItems SizeOption AlignOption
  | AlignSelf SizeOption AlignOption
  | AlignContent SizeOption AlignOption
    -- Sizing
  | W PercentOption
  | H PercentOption
  | MW PercentOption
  | MH PercentOption
  | VH PercentOption
  | VW PercentOption
  | MinVH PercentOption
  | MinVW PercentOption
  | M SideOption SizeOption GapOption
  | P SideOption SizeOption GapOption
    -- Image
  | ImgFluid
  | ImgThumbnail
    -- Table
  | Table [TableOption]
  | TableResponsive SizeOption
  | THeadDark
  | THeadLight
  | TableContext ContextOption
    -- Figure
  | Figure
  | FigureImg
  | FigureCaption
    -- Display (Typography)
  | Display1
  | Display2
  | Display3
  | Display4
    -- Lead
  | Lead
    -- Blockquote
  | Blockquote
  | BlockquoteFooter
    -- Text
  | TextAlign FloatSize TextAlignOption
  | TextWrap
  | TextNowrap
  | TextTruncate
  | TextBreak
  | TextMuted
  | TextUppercase
  | TextLowercase
  | TextCapitalize
  | TextMonospace
  | TextReset
  | TextDecorationNone
  | TextContext ContextOption
    -- Font
  | FontItalic
  | FontWeight FontWeightOption
    -- Lists
  | ListUnstyled
  | ListInline
  | ListInlineItem
    -- Alert
  | Alert ContextOption
  | AlertLink
  | AlertHeading
  | AlertDismissable
    -- Badge
  | Badge ContextOption
  | BadgePill ContextOption
    -- Breadcrumb
  | Breadcrumb
  | BreadcrumbItem
    -- Button
  | Btn ButtonType ContextOption ButtonSizeOption
    -- Button Group
  | BtnToolbar
  | BtnGroup ButtonGroupSizeOption
  | BtnGroupVertical
    -- Card
  | Card
  | CardBody
  | CardTitle
  | CardHeader
  | CardFooter
  | CardSubtitle
  | CardText
  | CardImgTop
  | CardLink
  | CardDeck
  | CardColumns
  | CardGroup
    -- Carousel
  | Carousel
  | CarouselInner
  | CarouselItem
  | CarouselCaption
  | CarouselFade
  | CarouselIndicators
  | CarouselControlPrev
  | CarouselControlNext
  | CarouselControlPrevIcon
  | CarouselControlNextIcon
    -- Collapse
  | Collapse
  | MultiCollapse
  | Collapsed
  | Accordion
    -- Dropdown
  | Dropdown
  | DropUp
  | DropRight
  | DropLeft
  | DropdownToggle
  | DropdownToggleSplit
  | DropdownMenu
  | DropdownMenuRight SizeOption
  | DropdownMenuLeft SizeOption
  | DropdownHeader
  | DropdownItem
  | DropdownDivider
    -- Forms
  | FormInline
  | FormGroup
  | FormControlLabel
  | FormControl SizeOption
  | FormControlPlaintext
  | FormText
  | FormControlFile
  | FormControlRange
  | FormCheck
  | FormCheckInline
  | FormCheckInput
  | FormCheckLabel
  | FormRow
  | ColFormLabel SizeOption
  | FormSelect
    -- Validation
  | ValidFeedback
  | InvalidFeedback
  | ValidTooltip
  | InvalidTooltip
    -- Input Group
  | InputGroup SizeOption
  | InputGroupPrepend
  | InputGroupAppend
  | InputGroupText
    -- Jumbotron
  | Jumbotron
  | JumbotronFluid
    -- List Group
  | ListGroup
  | ListGroupFlush
  | ListGroupHorizontal SizeOption
  | ListGroupItem ContextOption
  | ListGroupItemAction
    -- Media
  | Media
  | MediaBody
    -- Modal
  | Modal
  | ModalDialog ModalSize [ModalOption]
  | ModalContent
  | ModalHeader
  | ModalTitle
  | ModalBody
  | ModalFooter
    -- Nav
  | Nav [NavOption]
  | NavItem
  | NavLink
    -- Navbar
  | Navbar [NavbarOption]
  | NavbarBrand
  | NavbarToggler
  | NavbarTogglerIcon
  | NavbarText
  | NavbarCollapse
  | NavbarNav
    -- Tab
  | TabContent
  | TabPane
    -- Pagination
  | Pagination PaginationSize
  | PageItem
  | PageLink
    -- Progress
  | Progress
  | ProgressBar
  | ProgressBarStriped
  | ProgressBarAnimated
    -- Spinner
  | SpinnerBorder
  | SpinnerGrow
  | SpinnerBorderSM
  | SpinnerGrowSM
    -- Toast
  | Toast
  | ToastHeader
  | ToastBody
    -- Borders
  | Border BorderOption
  | BorderContext ContextOption
    -- Display (Visibility)
  | Display DisplaySize DisplayOption
    -- Embed
  | Embed21X9
  | Embed16X9
  | Embed4X3
  | Embed1X1
  | EmbedItem
    -- Float
  | FloatLeft FloatSize
  | FloatRight FloatSize
  | FloatNone FloatSize
    -- Position
  | Position PositionOption
    -- Shadows
  | Shadow
  | ShadowNone
  | ShadowSM
  | ShadowLG
    -- Util
  | OverflowAuto
  | OverflowHidden
  | TextHide
  | CloseIcon
  | ClearFix
  | Active
  | Disabled
  | FixedTop
  | FixedBottom
  | StickyTop
  | Fade
  | Show
  | Slide
  | SROnly
  | SROnlyFocusable
  | FlexNowrap
  | FlexColumn
  | Context ContextOption
  | Bg ContextOption
  | BgGradient ContextOption
  | StretchedLink
    -- Custom
  | Custom [Text]

data ButtonType
  = NormalButton
  | OutlineButton
  | LinkButton

data VerticalAlignOption
  = VerticalBaseline
  | VerticalTop
  | VerticalMiddle
  | VerticalBottom
  | TextToVerticalTop
  | TextToVerticalBottom

data FontWeightOption
  = BoldText
  | BolderText
  | NormalText
  | LightText
  | DarkText

data TextAlignOption
  = TextLeft
  | TextCenter
  | TextRight

data PositionOption
  = StaticPosition
  | RelativePosition
  | AbsolutePosition
  | FixedPosition
  | SitckyPosition

data FloatSize
  = FloatOnAll
  | FloatOnSmall
  | FloatOnMedium
  | FloatOnLarge
  | FloatOnExtraLarge

data BorderOption
  = AllBorders
  | TopBorder
  | RightBorder
  | BottomBorder
  | LeftBorder
  | NoBorder
  | NoTopBorder
  | NoRightBorder
  | NoBottomBorder
  | NoLeftBorder
  | Rounded
  | TopRounded
  | RightRounded
  | BottomRounded
  | LeftRounded
  | CircleRounded
  | PillRounded
  | NoRounded
  | RoundedSmall
  | RoundedLarge

data DisplayOption
  = DisplayNone
  | DisplayInline
  | DisplayInlineBlock
  | DisplayBlock
  | DisplayTable
  | DisplayCell
  | DisplayRow
  | DisplayFlex
  | DisplayInlineFlex

data DisplaySize
  = DisplayDefault
  | DisplayOnSmall
  | DisplayOnMedium
  | DisplayOnLarge
  | DisplayOnExtraLarge
  | DisplayOnPrint

data PaginationSize = Paginate | PaginateSmall | PaginateLarge
data NavOption = NavTabs | NavPills | NavFill | NavJustified
data ModalSize = MediumModal | SmallModal | LargeModal | ExtraLargeModal
data ModalOption = ModalCenter | ModalScroll

data ButtonSizeOption
  = SmallButton
  | MediumButton
  | LargeButton
  | BlockButton

data NavbarOption
  = DarkNavbar
  | LightNavbar
  | ExpandSmall
  | ExpandMedium
  | ExpandLarge
  | ExpandExtraLarge

data ButtonGroupSizeOption
  = ButtonGroupMedium
  | ButtonGroupSmall
  | ButtonGroupLarge

data ContextOption
  = NoContext
  | ActiveContext
  | PrimaryContext
  | SecondaryContext
  | SuccessContext
  | DangerContext
  | WarningContext
  | InfoContext
  | LightContext
  | WhiteContext
  | DarkContext
  | BlackContext
  | TransparentContext
  | BodyContext

data TableOption
  = TableDark
  | TableStriped
  | TableBordered
  | TableBorderless
  | TableHover
  | TableSmall

data AlignOption
  = AlignStart
  | AlignEnd
  | AlignCenter
  | AlignBaseline
  | AlignStretch

data JusitfyOption
  = JusitfyStart
  | JusitfyEnd
  | JusitfyCenter
  | JusitfyBetween
  | JusitfyAround

data GapOption
  = Gap0
  | Gap1
  | Gap2
  | Gap3
  | Gap4
  | Gap5
  | GapAuto
  | GapNeg1
  | GapNeg2
  | GapNeg3
  | GapNeg4
  | GapNeg5

data SideOption
  = TopSide
  | BottomSide
  | LeftSide
  | RightSide
  | XSides
  | YSides
  | AllSides

data PercentOption
  = Percent100
  | Percent75
  | Percent50
  | Percent25
  | PercentAuto

data SizeOption
  = NoSize
  | ExtraSmall
  | Small
  | Medium
  | Large
  | ExtraLarge

data BreakOption
  = NoBreak
  | Break0
  | Break1
  | Break2
  | Break3
  | Break4
  | Break5
  | Break6
  | Break7
  | Break8
  | Break9
  | Break10
  | Break11
  | Break12
  | BreakAuto
