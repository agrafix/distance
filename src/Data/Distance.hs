module Data.Distance
  ( Distance
  , meters, centimeters, millimeters, kilometers
  , toMeters, toCentimeters, toMillimeters, toKilometers
  )
where

-- | An abstract distance. Use the provided smart constructors to create
-- a meaningful distance. Note that on first sight a `Num` instance might
-- seem desirable, but this would defeat the purpose of having transparent
-- and explicitly constructed distances due to `fromInteger`.
newtype Distance
  = Distance { unDistance :: Double } -- as meters
  deriving (Show, Eq, Ord)

-- | An empty `Distance` is 0, and `mappend` is defined as addition
instance Monoid Distance where
    mempty = Distance 0
    mappend (Distance a) (Distance b) = Distance (a + b)

meters :: Double -> Distance
meters = Distance

centimeters :: Double -> Distance
centimeters = meters . (/ 100)

millimeters :: Double -> Distance
millimeters = centimeters . (/ 10)

kilometers :: Double -> Distance
kilometers = meters . (* 1000)

toMeters :: Distance -> Double
toMeters = unDistance

toCentimeters :: Distance -> Double
toCentimeters = (*100) . toMeters

toMillimeters :: Distance -> Double
toMillimeters = (*10) . toCentimeters

toKilometers :: Distance -> Double
toKilometers = (/1000) . toMeters
