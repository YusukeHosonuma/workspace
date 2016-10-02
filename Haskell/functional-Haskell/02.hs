
type Distance = Double

data Position = Position Double Double deriving (Show)

data Ship = Ship { position :: Position
                 , firingRange :: Distance
                 , unsafeRange :: Distance } deriving (Show)

type Region = Position -> Bool

inRange :: Position -> Distance -> Bool
inRange (Position x y) range = sqrt (x * x + y * y) <= range

length' :: Position -> Double
length' (Position x y) = sqrt $ x * x + y * y

minus :: Position -> Position -> Position
minus (Position x1 y1) (Position x2 y2) = Position (x1 - x2) (y1 - y2)

circle :: Distance -> Region
circle radius = (radius >=) . length'

shift :: Region -> Position -> Region
shift region offset = region . (flip minus) offset

invert :: Region -> Region
invert = (.) not

intersection :: Region -> Region -> Region
intersection r1 r2 = (&&) <$> r1 <*> r2

union :: Region -> Region -> Region
union r1 r2 = (||) <$> r1 <*> r2

difference :: Region -> Region -> Region
difference r1 r2 = intersection r1 $ invert r2

canSafelyEngageShip :: Ship -> Ship -> Ship -> Bool
canSafelyEngageShip self target friendly = result $ position target
    where
        unsafe    = circle $ unsafeRange self
        range     = difference (circle (firingRange self)) unsafe
        firing    = shift range $ position self
        friendlyR = shift unsafe $ position friendly
        result    = difference firing friendlyR
