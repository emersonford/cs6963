module MatchAction (combineMA, ActionResult (..)) where

import Protocol (Protocol)

-- Continue: continue down the chain of match-actions
-- Forward: skip subsequent match-actions, i.e. immediately send the packet to its destination
-- Error: do not forward the packet, send an error back to the client
data ActionResult a = Continue a | Forward a | Error

chainMA :: ActionResult a -> (a -> ActionResult a) -> ActionResult a
chainMA (Continue x) f = f x
chainMA (Forward x) _ = Forward x
chainMA Error _ = Error

makeMA :: (a -> Bool) -> (a -> ActionResult a) -> a -> ActionResult a
makeMA matchF actionF x = if matchF x then actionF x else Continue x

combineMA :: [(a -> Bool, a -> ActionResult a)] -> (a -> ActionResult a)
combineMA l x = foldl chainMA (uncurry makeMA (head l) x) (map (uncurry makeMA) (tail l))
