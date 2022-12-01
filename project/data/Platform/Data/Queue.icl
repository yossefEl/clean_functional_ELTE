implementation module Data.Queue

import StdEnv
import StdOverloadedList

newQueue :: Queue a
newQueue = Queue [|] [|]

instance length Queue where length (Queue front rear) = Length front + Length rear

enqueue :: a !(Queue a) -> Queue a
enqueue x (Queue front rear) = Queue front [|x:rear]

dequeue :: !(Queue a) -> (!?a, !Queue a)
dequeue q=:(Queue [|] [|]) = (?None, q)
dequeue (Queue [|x:xs] rear) = (?Just x, Queue xs rear)
dequeue (Queue [|] rear) = let [|x:xs] = Reverse rear in (?Just x, Queue xs [|])

toList :: !(Queue a) -> [a]
toList (Queue front rear) = [x \\ x <|- front] ++ reverse [x \\ x <|- rear]
