module Game.Poker
    ( module Game.Poker.Hands
    , module Game.Poker.Cards
    , simpleGame
    ) where
import System.Random.Shuffle

import Game.Poker.Hands
import Game.Poker.Cards

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe
import Safe

-----------
-- ハンドの入れ替え

type DiscardList = [Card] -- 捨て札
type Deck = [Card]        -- 山札

getHand :: Deck -> Maybe (Hand, Deck)
getHand deck = do
  hand <- toHand . take 5 $ deck
  return (hand, drop 5 deck)

drawHand :: Deck -> DiscardList -> Hand -> Maybe (Hand, Deck)
drawHand deck dis h = let
  nl = filter (flip notElem dis) (fromHand h)
  nr = drop (5 - length nl) deck
  in (,) <$> toHand (take 5 $ nl ++ deck) <*> Just nr

getDiscardList :: Hand -> IO (Maybe DiscardList)
getDiscardList h = do
    input <- getLine
    return $ do
      intList <- toIntList input
      res <- selectByIndexes (fromHand h) intList
      return res

------
-- helper

toIntList :: String -> Maybe [Int]
toIntList str = if and $ map isDigit str then Just $ reads str else Nothing
  where
    reads :: String -> [Int]
    reads = map $ read . (:[])

selectByIndexes :: [a] -> [Int] -> Maybe [a]
selectByIndexes l = sequence . map ((atMay l).(subtract 1))

-----------
-- AIの思考ルーチン(カードの入れ替え)

aiSelectDiscards :: Hand -> DiscardList
aiSelectDiscards hand = 
  case straightHint hand `mplus` flushHint hand *> Just [] of 
    Nothing -> nOfKindDiscards hand
    Just xs -> xs 

nOfKindDiscards :: Hand -> DiscardList
nOfKindDiscards hand = filter (flip notElem $ allNOfKinds hand) $ fromHand hand
  where
    allNOfKinds :: Hand -> [Card]
    allNOfKinds hand = concat . concat 
      $ catMaybes [nOfKindHint 2 hand, nOfKindHint 3 hand, nOfKindHint 4 hand]

-----------
-- 勝敗判定

judgeVictory :: (PokerHand, Card) -> (PokerHand, Card) -> Ordering
judgeVictory l r = compare (pullStrength l) (pullStrength r)
  where
    pullStrength :: (PokerHand, Card) -> (PokerHand, Int)
    pullStrength = fmap cardStrength

-----------
-- プロトタイプ

simpleGame :: IO ()
simpleGame  = do
  putStrLn "------------------"
  putStrLn "-- simple poker --"
  putStrLn "------------------"
  deck <- shuffleM allCards
  case getHand deck of
    Nothing -> error "予期せぬエラー : getHand in simpleGame"
    Just res -> matchPoker res
  ynQuestion "-- もっかいやる？" simpleGame (putStrLn "-- またねノシノシ")

--------

data Player = Player | Enemy deriving Eq

showPlayerName :: Player -> String
showPlayerName Player = "あなた"
showPlayerName Enemy = "あいて"

matchPoker :: (Hand, Deck) -> IO ()
matchPoker (mhand, deck) = do
  (mres, ndeck, nmhand) <- playPoker mhand deck Player
  case getHand ndeck of
    Nothing -> error "予期せぬエラー : getHand in matchPoker"
    Just (ehand, odeck) -> do
      (eres, _, nehand) <- playPoker ehand odeck Enemy
      printResult nmhand nehand mres eres
  
playPoker :: Hand -> Deck -> Player -> IO ((PokerHand, Card), Deck, Hand)
playPoker hand deck player = do
  discards <- if player == Player 
    then inputDisuse hand
    else aiDisuse hand
  case drawHand deck discards hand of
    Nothing -> error "予期せぬエラー : drawHand"
    Just (nhand, ndeck) -> do
      let res = pokerHand nhand
      return (res, ndeck, nhand)

inputDisuse :: Hand -> IO DiscardList
inputDisuse hand = do
  printHand [] hand Player
  putStrLn "-- 捨てるカードを選んでね"
  gotDisuse <- getDiscardList hand
  case gotDisuse of
    Nothing -> do
      putStrLn "-- 1~5の数値を並べて入力してね"
      inputDisuse hand
    Just disuses -> do
      printHand disuses hand Player
      ynQuestion "-- あなた：これでいい？" (return disuses) (inputDisuse hand)

aiDisuse :: Hand -> IO DiscardList
aiDisuse hand = do
  let res = aiSelectDiscards hand
  printHand res hand Enemy
  putStrLn "-- あいて：これでいいよ！" 
  return res

----
          
printResult :: Hand -> Hand -> (PokerHand, Card) -> (PokerHand, Card) -> IO ()
printResult mhand ehand mres@(mph, mcard) eres@(eph, ecard) = do
  putStrLn " ***** 結果発表！！ *****"
  printHand [] mhand Player
  printHand [] ehand Enemy
  putStrLn $ concat ["あなたの手札は ", show mph, " で、最強カードは ", show mcard, " でした"]
  putStrLn $ concat ["あいての手札は ", show eph, " で、最強カードは ", show ecard, " でした"]
  case judgeVictory mres eres of
    LT -> putStrLn "あなたの負けです"
    EQ -> putStrLn "引き分けです"
    GT -> putStrLn "あなたの勝ちです"

printHand :: DiscardList -> Hand -> Player -> IO ()
printHand dis hand player = 
  putStrLn $ "-- " ++ showPlayerName player ++ "の手札 : " ++ showChangeHand dis hand

ynQuestion :: String -> IO a -> IO a -> IO a
ynQuestion s yes no = do
  putStrLn $ s ++ "(y/n)"
  input <- getLine
  case input of 
    "y" -> yes
    "n" -> no
    _ -> do
      putStrLn "-- `y`か`n`で入力してね"
      ynQuestion s yes no

showChangeHand :: DiscardList -> Hand -> String
showChangeHand dis h = let
  judge x = if elem x dis then " " ++ show x ++ " " else "[" ++ show x ++ "]"
  in concat $ map judge (fromHand h)
      
