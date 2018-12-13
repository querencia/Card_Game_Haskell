-- Defines Card Data Type
--
-- Cards are ranked based on their Rank with A being the top valued card.
-- Suits are ranked as follows (from low to high):
--                  Spades -> Diamonds -> Clubs -> Hearts

module Cards where

import System.Random
import Data.List ( sortBy )
import Data.Ord ( comparing )

data Value = C2 | C3 | C4 | C5 | C6 |
            C7 | C8 | C9 | C10 |
            J | Q | K | A deriving (Enum, Eq, Ord)

instance Show Value where
    show A  = "A"
    show J  = "J"
    show Q  = "Q"
    show K  = "K"
    show v  = show ((fromEnum v) + 2)

data Suit = Spades | Diamonds | Clubs | Hearts deriving (Show, Enum, Eq, Ord)

data Card = Card Value Suit deriving (Eq)

instance Show Card where
    show (Card v s) = (show v) ++ " of " ++ (show s)

instance Ord Card where
    (Card v1 s1) `compare` (Card v2 s2) = if val == EQ
                                            then (s1 `compare` s2)
                                            else val where
                                            val = (v1 `compare` v2)

instance Enum Card where
    fromEnum (Card v s) = 4 * (fromEnum v) + (fromEnum s)
    toEnum p = Card (toEnum (floor ((fromIntegral p) / 4))) (toEnum (p `mod` 4))

-- Probability that the card chosen is the best
getProbabilityPerCard :: Card -> Double
getProbabilityPerCard c = (fromIntegral (fromEnum c)) / 51

-- Sort deck using a random generator
sortedDeck :: [Card]
sortedDeck = fmap toEnum [0 .. 51]

shuffle :: StdGen -> [a] -> [a]
shuffle g es = map snd $ sortBy (comparing fst) $ zip rs es
               where rs = randoms g :: [Int]

do_shuffle :: IO [Card]
do_shuffle = do gen <- getStdGen
                return (shuffle gen sortedDeck)

-- Deal n cards to player
deal :: Int -> [Card] -> ([Card], [Card])
deal n deck = splitAt n deck

do_deal:: Int -> [Card] -> IO ([Card], [Card])
do_deal n deck = do return (deal n deck)
