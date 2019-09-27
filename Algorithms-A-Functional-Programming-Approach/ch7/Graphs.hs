module Graph (mkGraph, adjacent, nodes, edgesD, edgesU, edgeIn, weight) where
  import Data.Array
  import Queue
  import Stack
  import PQueue
  mkGraph :: (Ix n, Num w) => Bool -> (n,n) -> [(n,n,w)] -> (Graph n w)
  adjacent :: (Ix n, Num w) => (Graph n w) -> n -> [n]
  nodes :: (Ix n, Num w) => (Graph n w) -> [n]
  edgesD, edgesU :: (Ix n, Num w) => (Graph n w) -> [(n,n,w)]
  edgeIn :: (Ix n, Num w) => (Graph n w) -> (n,n) -> Bool
  weight :: (Ix n, Num w) => n -> n -> (Graph n w) -> w

-- Adjacency Array Representation. Access node in O(1)
  type Graph n w = Array n [(n,w)]

  mkGraph dir bns es = 
      accumArray (\xs x -> x:xs) [] bns
      ([(x1,(x2,w)) | (x1,x2,w) <- es] ++
  	if dir then [] -- add two way edges for non directed graphs
  	else [(x2,(x1,w)) | (x1,x2,w) <- es, x1 /= x2])
  
  adjacent g v = map fst (g!v) 
  
  nodes g = indices g
  
  edgeIn g (x,y) = elem y (adjacent g x)
  
  weight x y g = head [c | (a,c) <- g!x, a == y]
  
  edgesD g = [(v1,v2,w) | v1 <- nodes g, (v2,w) <- g!v1]
  
  edgesU g = [(v1,v2,w) | v1 <- nodes g, (v2,w) <- g!v1, v1 < v2] -- edges in undirected graph

{-- Adjacency Array Representation. Access edge in O(1), access adjacency list in O(V).
  type Graph n w = Array (n,n) (Maybe w)

  mkGraph dir bnds@(l,u) es =
      emptyArray // ([((x1,x2), Just w) | (x1,x2,w) <- es] ++
	  if dir then []
	  else [((x2,x1), Just w) | (x1,x2,w) <- es, x1 /= x2])
      where emptyArray = array ((1,1),(u,u)) [((x1,x2), Nothing) | x1 <- range bnds, x2 <- range bnds]

  adjacent g v1 = [v2 | v2 <- nodes g, (g!(v1,v2)) /= Nothing]

  nodes g = range (1,u) 
    where ((1,_),(u,_)) = bounds g

  edgeIn g (x,y) = (g!(x,y)) /= Nothing

  weight x y g = w 
    where (Just w) = g!(x,y)

  edgesD g = [(v1, v2, unwrap (g!(v1,v2))) | v1 <- nodes g, v2 <- nodes g, edgeIn g (v1,v2)]
    where unwrap (Just w) = w

  edgesU g = [(v1, v2, unwrap (g!(v1,v2))) | v1 <- nodes g, v2 <- range (v1,u), edgeIn g (v1,v2)]
    where (_,(u,_)) = bounds g
	  unwrap (Just w) = w

--}

-- Depth first search
  depthFirstSearch :: (Ix a, Num w) => a -> Graph a w -> [a]
  depthFirstSearch start g = dfs (push start emptyStack) []
    where
      dfs s vis 
        | (stackEmpty s ) = vis
        | elem (top s) vis = dfs (pop s) vis
        | otherwise = let c = top s 
  		      in 
  		       dfs (foldr push (pop s) (adjacent g c)) (vis ++ [c])
  
 -- Breath first search
  breathFirstSearch :: (Ix a, Num w) => a -> Graph a w -> [a]
  breathFirstSearch start g = bfs (enqueue start emptyQueue) []
    where 
      bfs q vis 
        | (queueEmpty q) = vis
        | elem (front q) vis = bfs (dequeue q) vis
        | otherwise = let c = front q
 		      in
 		       bfs (foldr enqueue (dequeue q) (adjacent g c)) (vis ++ [c])

-- Topological Sort
  inDegree g n = length [t | v <- nodes g, t <- adjacent g v, n == t]
  
  topologicalSort g = tsort [n | n <- nodes g, inDegree g n == 0] []
    where 
      tsort [] r = r
      tsort (c:cs) vis
        | elem c vis = tsort cs vis
        | otherwise = tsort cs (c:(tsort (adjacent g c) vis))
  
