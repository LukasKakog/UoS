
data VTree a = Leaf | Node (VTree a) a Int (VTree a) deriving Show
data Direction a = Lt a Int (VTree a) | Rt a Int (VTree a) deriving Show
type Trail a = [Direction a]
type Zipper a = (VTree a, Trail a)

decide :: (Ord a) => a -> Zipper a -> Zipper a
decide x ((Leaf),trail) = insert x ((Leaf),trail) 
decide x ((Node vt1 v n vt2),[]) = insert x ((Node vt1 v n vt2),[])
decide x ((Node vt1 v n vt2), trail) | x == v = ((Node vt1 v n vt2), trail)
                                     | null trail = insert x ((Node vt1 v n vt2), trail)
                                     | otherwise = help $ moveUp ((Node vt1 v n vt2), trail)
                                          where help ((Node vt3 a b vt4), tr) | x < a && x > v = insert x ((Node vt3 a b vt4), tr)
                                                                              | otherwise = decide x ((Node vt3 a b vt4), tr)

insert :: (Ord a) => a -> Zipper a -> Zipper a
insert x ((Leaf),trail) = (Node Leaf x 1 Leaf,trail)
insert x ((Node vt1 v n vt2),trail) | v == x = (Node vt1 v n vt2,trail)
                                    | v < x = insert x $ moveRight ((Node vt1 v n vt2),trail)
                                    | v > x = insert x $ moveLeft ((Node vt1 v n vt2),trail)

moveUp :: (Ord a) => Zipper a -> Zipper a
moveUp (vt, (Lt v n t : trail)) = (Node (vt) v (n+1) (t), trail)
moveUp (vt, (Rt v n t : trail)) = (Node (t) v (n+1) (vt), trail)

moveLeft :: (Ord a) => Zipper a -> Zipper a 
moveLeft ((Node vt1 v n vt2), ts) = (increment vt1, Lt v n vt2 : ts)

moveRight :: (Ord a) => Zipper a -> Zipper a
moveRight ((Node vt1 v n vt2), ts) = (increment vt2, Rt v n vt1 : ts)

increment :: (Ord a) => VTree a -> VTree a
increment Leaf = Leaf
increment (Node vt1 v n vt2) = (Node vt1 v (n+1) vt2)

contains :: (Ord a) => a -> VTree a -> Bool
contains _ Leaf = False
contains x (Node vt1 v n vt2) | x == v = True
                              | x < v = contains x vt1
                              | x > v = contains x vt2

insertFromCurrentNode :: Ord a => a -> Zipper a -> Zipper a
insertFromCurrentNode item zs | contains item (fst zs) = zs
                              | otherwise = decide item zs 
                              
{-
leftToRoot :: (Ord a) => Zipper a -> Zipper a 
leftToRoot (vt,[]) = (vt,[])
leftToRoot (vt,(Rt pv pn pt : trail)) = (vt,(Rt pv pn pt : trail)) 
leftToRoot (vt,(Lt pv pn pt : trail)) = leftToRoot ((fst $ moveUp (vt,[Lt pv pn pt])),trail)

rightToRoot :: (Ord a) => Zipper a -> Zipper a 
rightToRoot (vt,[]) = (vt,[])
rightToRoot (vt,(Lt pv pn pt : trail)) = (vt,(Lt pv pn pt : trail)) 
rightToRoot (vt,(Rt pv pn pt : trail)) = rightToRoot ((fst $ moveUp (vt,[Rt pv pn pt])),trail)

retrieve :: (Ord a) => VTree a -> a
retrieve (Node vt1 v n vt2) = v

retrieveParent :: (Ord a) => Zipper a -> a 
retrieveParent (vt,ts) = retrieve (moveUp ts vt)


decide x ((Node vt1 v n vt2),trail) | v == x = insert x (increment (Node vt1 v n vt2),trail)
-}

--for testing--
mkTree :: Ord a => [a] -> Zipper a
mkTree = foldl (\z -> \x -> insertFromCurrentNode x z) (Leaf,[])

zipper1 = (vtree1,trail1)
zipper3 = (vtree3,trail3)

vtree1 = Node (Node Leaf 1 1 Leaf) 3 3 (Node (Node Leaf 4 1 Leaf) 5 2 Leaf)
vtree2 = Node (Node Leaf 10 2 Leaf) 13 3 Leaf
vtree3 = Node Leaf 2 1 Leaf

trail1 = [d1,d2]
trail3 = [d3,d4,d5]

d1 = Lt 8 5 vtree2
d2 = Rt 0 9 Leaf

d3 = Lt 3 2 Leaf
d4 = Lt 4 4 Leaf
d5 = Lt 5 3 Leaf