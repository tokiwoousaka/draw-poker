module Game.Poker.AI where
import Data.Maybe
import Control.Applicative
import Control.Monad

import Game.Poker.Cards
import Game.Poker.Hands

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

