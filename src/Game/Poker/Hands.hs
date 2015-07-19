module Game.Poker.Hands 
  ( Hand
  , toHand, fromHand
  , PokerHand(..)
  , pokerHand 
  ----
  -- hint
  , straightHint
  , flushHint
  , nOfKindHint
  ----
  -- hand
  , straightFlush
  , fourOfAKind
  , fullHouse
  , flush
  , straight
  , threeOfAKind
  , twoPair
  , onePair 
  ) where
import Game.Poker.Cards
import Data.List
import Control.Monad

newtype Hand = Hand { fromHand :: [Card] } deriving (Show, Eq, Ord)

toHand :: [Card] -> Maybe Hand
toHand l = 
  if length l == 5 
    then Just $ Hand (sort l)
    else Nothing

pokerHand :: Hand -> (PokerHand, Card)
pokerHand h@(Hand l) = 
    case foldl mplus Nothing $ fmap ($h) hands of
      Just pc -> pc
      Nothing -> (HighCards, last l)
  where
    hands :: [Hand -> Maybe (PokerHand, Card)]
    hands = 
      [ straightFlush
      , fourOfAKind
      , fullHouse
      , flush
      , straight
      , threeOfAKind
      , twoPair
      , onePair 
      ]

-------

-- ポーカー・ハンド
data PokerHand 
  = HighCards
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | Straight
  | Flush
  | FullHouse
  | FourOfAKind
  | StraightFlush
  deriving (Show, Read, Eq, Ord, Enum)

-------
-- Hint

straightHint :: Hand -> Maybe Card
straightHint (Hand l) = 
  (judgeStright . extract cardStrength $ l)
  `mplus`
  (judgeStright . sort . extract cardNumber $ l)
    where
      isStright :: [Int] -> Bool
      isStright xs@(x:_) = xs == [x .. x + 4]
      isStright _ = False
      
      judgeStright :: [(Int, Card)] -> Maybe Card
      judgeStright l = 
        if isStright $ map fst l
          then Just . snd . last $ l
          else Nothing

flushHint :: Hand -> Maybe Card
flushHint (Hand (x:xs)) = 
  if all ((cardSuit x==).cardSuit) xs then Just (last xs) else Nothing

nOfKindHint :: Int -> Hand -> Maybe [[Card]]
nOfKindHint n (Hand h) = if cards /= [] then Just cards else Nothing
  where
    cards :: [[Card]]
    cards = filter ((==n).length) 
      $ groupBy (\x y -> cardNumber x == cardNumber y) h

-------
-- PokerHand

straightFlush :: Hand -> Maybe (PokerHand, Card)
straightFlush h = do
  c <- straightHint  h
  d <- flushHint h
  return (StraightFlush, max c d)

fourOfAKind :: Hand -> Maybe (PokerHand, Card)
fourOfAKind h = do
  cs <- nOfKindHint 4 h
  return (FourOfAKind, last $ concat cs)

fullHouse :: Hand -> Maybe (PokerHand, Card)
fullHouse h = do
  cs <- nOfKindHint 3 h
  nOfKindHint 2 h
  return (FullHouse, last $ concat cs)

flush :: Hand -> Maybe (PokerHand, Card)
flush h = do
  c <- flushHint h
  return (Flush, c)

straight :: Hand -> Maybe (PokerHand, Card)
straight h = do
  c <- straightHint h
  return (Straight, c)

threeOfAKind :: Hand -> Maybe (PokerHand, Card)
threeOfAKind h = do
  cs <- nOfKindHint 3 h
  return (ThreeOfAKind, last $ concat cs)

twoPair :: Hand -> Maybe (PokerHand, Card)
twoPair h = do
  cs <- nOfKindHint 2 h
  if length cs == 2 
    then Just (TwoPair, last $ concat cs)
    else Nothing
  
onePair :: Hand -> Maybe (PokerHand, Card)
onePair h = do
  cs <- nOfKindHint 2 h
  return (OnePair, last $ concat cs)

-------
-- Helper

extract :: (b -> a) -> [b] -> [(a, b)]
extract f cs = map (\c -> (f c, c)) cs
