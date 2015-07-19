module Game.Poker.Cards 
  ( Suit(..) 
  , Card
  , allCards
  , cardSuit
  , cardNumber
  , cardStrength
  ----
  , h2 , h3 , h4 , h5 , h6 , h7 , h8 , h9 , h10 , hJ , hQ , hK , hA 
  , d2 , d3 , d4 , d5 , d6 , d7 , d8 , d9 , d10 , dJ , dQ , dK , dA 
  , c2 , c3 , c4 , c5 , c6 , c7 , c8 , c9 , c10 , cJ , cQ , cK , cA 
  , s2 , s3 , s4 , s5 , s6 , s7 , s8 , s9 , s10 , sJ , sQ , sK , sA 
  ) where
--import Control.DeepSeq

data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Show, Read, Eq, Ord, Enum)
data Card = Card Int Suit
  deriving (Eq, Ord)
instance Show Card where
  show (Card i Hearts) = "H" ++ showCardNumber i
  show (Card i Diamonds) = "D" ++ showCardNumber i
  show (Card i Clubs) = "C" ++ showCardNumber i
  show (Card i Spades) = "S" ++ showCardNumber i

showCardNumber :: Int -> String
showCardNumber 14 = "A_"
showCardNumber 13 = "K_"
showCardNumber 12 = "Q_"
showCardNumber 11 = "J_"
showCardNumber 10 = "10"
showCardNumber x = (show $ x) ++ "_"

allCards :: [Card]
allCards = [ Card num suit | suit <- [Hearts ..], num <- [2..14] ]

cardSuit :: Card -> Suit
cardSuit (Card _ s) = s

cardNumber :: Card -> Int
cardNumber (Card 14 _) = 1 -- Aは14なので
cardNumber (Card n _) = n

cardStrength :: Card -> Int
cardStrength (Card n _) = n

--------

h2 = Card 2 Hearts
h3 = Card 3 Hearts
h4 = Card 4 Hearts
h5 = Card 5 Hearts
h6 = Card 6 Hearts
h7 = Card 7 Hearts
h8 = Card 8 Hearts
h9 = Card 9 Hearts
h10 = Card 10 Hearts
hJ = Card 11 Hearts
hQ = Card 12 Hearts
hK = Card 13 Hearts
hA = Card 14 Hearts

d2 = Card 2 Diamonds
d3 = Card 3 Diamonds
d4 = Card 4 Diamonds
d5 = Card 5 Diamonds
d6 = Card 6 Diamonds
d7 = Card 7 Diamonds
d8 = Card 8 Diamonds
d9 = Card 9 Diamonds
d10 = Card 10 Diamonds
dJ = Card 11 Diamonds
dQ = Card 12 Diamonds
dK = Card 13 Diamonds
dA = Card 14 Diamonds

c2 = Card 2 Clubs
c3 = Card 3 Clubs
c4 = Card 4 Clubs
c5 = Card 5 Clubs
c6 = Card 6 Clubs
c7 = Card 7 Clubs
c8 = Card 8 Clubs
c9 = Card 9 Clubs
c10 = Card 10 Clubs
cJ = Card 11 Clubs
cQ = Card 12 Clubs
cK = Card 13 Clubs
cA = Card 14 Clubs

s2 = Card 2 Spades
s3 = Card 3 Spades
s4 = Card 4 Spades
s5 = Card 5 Spades
s6 = Card 6 Spades
s7 = Card 7 Spades
s8 = Card 8 Spades
s9 = Card 9 Spades
s10 = Card 10 Spades
sJ = Card 11 Spades
sQ = Card 12 Spades
sK = Card 13 Spades
sA = Card 14 Spades
