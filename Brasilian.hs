--  Playing Card Game
--  By Euxhen Hasanaj
--  
--  Four players are handed out 10 cards each.
--  Each player chooses a card from their hand and  predicts  whether
--  they have the best card  among  all  four.  After each prediction
--  has been made, the cards are revealed and the winner is selected.
--
--  Score distribution is as follows
--
--  Prediction     Winner     ScoreUpdate
--     YES          YES           +1
--     YES          NO            -1
--     NO           YES           -1
--     NO           NO             0

import System.Random
import System.IO

import Cards
import Data.List
import Data.Ord

bestCard :: [Card] -> (Card, Int)
bestCard xs = maximumBy (comparing fst) (zip xs [0..])
  
-- Needed to make game predictions based on probability per card winning
compareLists :: [Double] -> [Double] -> [Bool]
compareLists [] _ = []
compareLists _ [] = []
compareLists (f1:f1s) (f2:f2s) = [f1 >= f2] ++ compareLists f1s f2s

-- Distribution of points
checkResults :: [Bool] -> Int -> [Int]
checkResults [] _ = []
checkResults (c:cs) wi = if wi == 0 && c == True
                        then [1] ++ checkResults cs (wi-1)
                            else if wi == 0 && c == False
                            then [-1] ++ checkResults cs (wi-1)
                                else if c == True
                                then [-1] ++ checkResults cs (wi-1)
                                    else [0] ++ checkResults cs (wi-1)

-- Remove leading cards from NPC's
cutPcHands :: [[Card]] -> [[Card]]
cutPcHands [] = []
cutPcHands pcCards = fmap tail pcCards

-- Remove n-th card from user's hand
removeN :: Int -> [a] -> [a]
removeN _ []     = []
removeN i (a:as)
  | i == 0    = as
  | otherwise = a : removeN (i-1) as

-- Remove played cards on round
cutHands :: [[Card]] -> Int -> [[Card]]
cutHands hands myi = (cutPcHands (init hands)) ++ [(removeN myi (last hands))]

-- Play game
play :: [[Card]] -> [Int] -> StdGen -> IO ()
play [[], [], [], []] _ _ = return ()
play hands points stdGen = do 
    hSetBuffering stdout NoBuffering  -- Allow for output
    putStrLn "|-----------------------------------------------------|"
    putStrLn "|-----------------    New Turn    --------------------|"
    putStrLn "|-----------------------------------------------------|"

    putStrLn $ "\nCurrent Players: P1, P2, P3, You\n"
    let names = ["P1", "P2", "P3", "You"]

    -- Print player's hand
    let indexedHand = zip [0 .. ] (last hands)
    putStrLn $ "Your hand is: "
    mapM_ print indexedHand

    putStr $ "Choose a card index (starting from 0) : "

    -- Choose card to play
    char <- getLine
    let cardPicked = (last hands) !! (read char)

    -- Predict round outcome
    putStr "Do you think you can win? "
    ans <- getLine
    let myAns = if ans == "yes"
                then True
                else False

    -- Assign probabilities to cards
    let pcCards = (fmap head (reverse (tail (reverse hands))))
    let probabilities = fmap getProbabilityPerCard pcCards

    -- Choose predictions for NPC's
    let (i1, s2) = randomR (0, 1 :: Double) stdGen
    let (i2, s3) = randomR (0, 1 :: Double) s2
    let (i3, s4) = randomR (0, 1 :: Double) s3
    let randomFloats = [i1, i2, i3]

    let choices = compareLists probabilities randomFloats

    let allCards = pcCards ++ [cardPicked]
    let allChoices = choices ++ [myAns]

    -- Find winner
    let results = bestCard allCards
    let winningCard = fst results
    let winningIndex = snd results

    -- Update points and hands
    let pointResults = checkResults allChoices winningIndex

    putStrLn "\n---------Cards are:"
    mapM_ print (zip names allCards)
    putStrLn "\n---------Choices by players are:"
    mapM_ print (zip names allChoices)
    putStrLn "\n---------Results of the turn are:"
    mapM_ print (zip names pointResults)
    putStrLn ""

    let newPts = fmap (\a -> (points !! a) + (pointResults !! a)) [0 .. (length points) - 1]
    putStrLn "\nCurrent points are:"
    mapM_ print (zip names newPts)
    putStrLn ""

    play (cutHands hands (read char)) newPts s4

    return ()

-- Deal cards and start first round
startGame :: StdGen -> IO ()
startGame stdGen = do
    putStrLn ("\nNew Game")
    putStrLn ("Trump is shuffling the deck\n")
    deck <- do_shuffle
    (hand1, deck) <- do_deal 10 deck
    (hand2, deck) <- do_deal 10 deck
    (hand3, deck) <- do_deal 10 deck
    (hand4, deck) <- do_deal 10 deck
    play [hand1, hand2, hand3, hand4] [0, 0, 0, 0] stdGen

main = do 
    stdGen <- getStdGen 
    startGame stdGen
