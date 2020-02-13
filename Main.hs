module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- NOTE: All Ints should be Nats. Should use smart constructors and not export
-- data constructors.

data Position = Position { _posX :: Int, _posY :: Int }
  deriving (Show, Eq, Ord)

isPos :: Int -> Maybe Int
isPos x = if x > 0 then Just x else Nothing

mkPosition :: Int -> Int -> Maybe Position
mkPosition (isPos -> x) (isPos -> y) = Position <$> x <*> y

data Hole = Hole { _holeRadius :: Int, _holeDepth :: Int, _holePosition :: Position}
  deriving (Show, Eq, Ord)

mkHole :: Int -> Int -> Position -> Maybe Hole
mkHole (isPos -> rad) (isPos -> dep) pos = Hole <$> rad <*> dep <*> pure pos

data Panel = Panel { _panelWidth :: Int, _panelLength :: Int, _panelThickness :: Int, _panelHolePattern :: [Hole] }
  deriving (Show, Eq, Ord)

data CabinetDims = CabinetDims { _cabinetWidth :: Int, _cabinetHeight :: Int, _cabinetDepth :: Int, _cabinetThickness :: Int }

-- Cabinet Properties
data Style = Overlay | HalfOverlay | Inset
data BoxConstruction = FreeStanding | SharedWall
data Indexing = System | Shifted
data Base = IntegralToeKick | LevelingFeet
newtype BaseHeight = BaseHeight Int
newtype Gap = Gap Int
newtype Reveal = Reveal Int
newtype FrontOffest = FrontOffset Int
newtype RearOffset = RearOffset Int
newtype HingeOffset = HingeOffset Int -- Hinge Cup offset. Should also include cup size and depth?
newtype HasCountertop = HasCountertop Bool
newtype StrainerWidth = StrainerWidth Int
newtype NumOfBoxes = NumOfBoxes Int
data BoxWidth = BWRatio Int | BWExplicit Int
newtype BoxWidths = BoxWidths [BoxWidth]

data CabinetSpec = CabinetSpec
  { _dims :: CabinetDims -- NOTE: Should this be overall dims or per box??
  , _hasCntr :: HasCountertop -- NOTE: Should all optional properties be delegated to a list?
  , _boxCnstr :: BoxConstruction
  , _base :: Base
  , _baseHeight :: BaseHeight
  , _style :: Style
  , _gap :: Gap
  , _reveal :: Reveal
  , _starts :: Starts -- TODO: What is this again?
  , _indexing :: Indexing
  , _rearOffset :: RearOffset
  , _strainerWidth :: StrainerWidth
  , _boxWidths :: BoxWidths
  }

data Label = Upright | RearUpright | Bottom | Strainer | Countertop | Face | Toekick
  deriving (Show, Eq)

