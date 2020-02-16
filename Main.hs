module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- NOTE: All Ints should be Nats. Should use smart constructors and not export
-- data constructors.

newtype Natural = Natural Int
  deriving (Show, Eq, Ord)

mkNat :: Int -> Maybe Natural
mkNat (isPos -> x) = Natural <$> x

isPos :: Int -> Maybe Int
isPos x = if x > 0 then Just x else Nothing

data Position = Position { _posX :: Natural, _posY :: Natural }
  deriving (Show, Eq, Ord)

mkPosition :: Natural -> Natural -> Position
mkPosition = Position

data Hole = Hole { _holeRadius :: Natural, _holeDepth :: Natural, _holePosition :: Position}
  deriving (Show, Eq, Ord)

mkHole :: Natural -> Natural -> Position -> Hole
mkHole = Hole

data Panel = Panel { _panelWidth :: Natural, _panelLength :: Natural, _panelThickness :: Natural, _panelHolePattern :: [Hole] }
  deriving (Show, Eq, Ord)

data CabinetDims = CabinetDims
  { _cabinetWidth :: Natural
  , _cabinetHeight :: Natural
  , _cabinetDepth :: Natural
  , _cabinetMaterialThickness :: Natural
  }

-- Cabinet Properties
data Style = Overlay | HalfOverlay | Inset
data BoxConstruction = FreeStanding | SharedWall
data Indexing = System | Shifted
data Base = IntegralToeKick | LevelingFeet
newtype BaseHeight = BaseHeight Natural
newtype Gap = Gap Natural
newtype Reveal = Reveal Natural
newtype FrontOffest = FrontOffset Natural
newtype RearOffset = RearOffset Natural
newtype HingeOffset = HingeOffset Natural -- Hinge Cup offset. Should also include cup size and depth?
newtype HoleOffset = HoleOffset Natural
newtype HasCountertop = HasCountertop Bool
newtype StrainerWidth = StrainerWidth Natural
newtype NumOfBoxes = NumOfBoxes Natural
newtype BoxWidths = BoxWidths [Natural]

data CabinetSpec = CabinetSpec
  { _dims :: CabinetDims -- Overall Dimensions
  -- , _hasCntr :: HasCountertop -- NOTE: Should all optional properties be delegated to a list?
  , _boxCnstr :: BoxConstruction       -- Assume FreeStanding
  , _base :: Base                      -- Assume LevelFeet
  , _baseHeight :: BaseHeight
  -- , _style :: Style                 -- Assume Overlay
  , _gap :: Gap
  , _reveal :: Reveal
  , _starts :: (HoleOffset, HoleOffset)
  , _indexing :: Indexing              -- Assume System
  , _rearOffset :: RearOffset
  --, _strainerWidth :: StrainerWidth
  , _boxWidths :: BoxWidths
  }

data Label = Upright | RearUpright | Bottom | Strainer | Countertop | Face | Toekick
  deriving (Show, Eq)

newtype Cabinet = Cabinet [(Label, Panel, Position)]
type CabinetUnit = [Cabinet]

genCabinets :: CabinetSpec -> CabinetUnit
genCabinets = undefined
