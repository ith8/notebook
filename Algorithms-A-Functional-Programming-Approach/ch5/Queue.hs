module Queue (Queue, enqueue, dequeue, front, emptyQueue, queueEmpty) where 
  enqueue :: a -> Queue a -> Queue a
  dequeue :: Queue a -> Queue a
  front :: Queue a -> a
  emptyQueue :: Queue a
  queueEmpty :: Queue a -> Bool
  
  newtype Queue a   = Q [a]
     deriving Show
  
  emptyQueue     = Q []
  
  queueEmpty (Q [])  = True
  queueEmpty (Q _ )  = False
  
  enqueue x (Q q)    = Q (q ++ [x])
  
  dequeue (Q (_:xs)) = Q xs
  dequeue (Q [])     = error "dequeue: empty queue"
  
  front (Q (x:_)) = x
  front (Q [])    = error "front: empty queue"
