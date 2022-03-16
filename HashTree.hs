-- rk418291
module HashTree where
import Hashable32


data Tree a =
        Leaf Hash a
        | Node Hash (Tree a) (Tree a)

-- returns True if the Tree has two children, false otherwise
hasTwoChildren :: Tree a -> Bool
hasTwoChildren (Node _ l r) =
    treeHash l /= treeHash r
hasTwoChildren (Leaf _ _) = False

leaf :: Hashable a => a -> Tree a
leaf a = Leaf (hash a) a

twig :: Hashable a => Tree a -> Tree a
twig t =
    Node (hash (val, val)) t t
    where
        val = treeHash t

node :: Hashable a =>  Tree a -> Tree a -> Tree a
node t1 t2 =
    Node (hash (val1, val2)) t1 t2
    where
        val1 = treeHash t1
        val2 = treeHash t2


buildTree :: Hashable a => [a] -> Tree a
buildTree l =
    head $ rec_aux (map leaf l)
    where
        aux (x:y:xs) =
            (node x y):(aux xs)
        aux [x] =
            [twig x]
        aux [] = []
        
        rec_aux l
            | length res == 1 = res
            | otherwise = rec_aux res
          where
              res = aux l

treeHash :: Tree a -> Hash
treeHash (Node h _ _) = h
treeHash (Leaf h _) = h


instance Show a => Show (Tree a) where
    -- prec here is used to count the indent
    showsPrec prec (Leaf h a) =
        (spaces prec)
        .(showsHash h)
        .(showString " ")
        
        .(shows a)
        .(showString "\n")
    
    showsPrec prec t@(Node h l r) =
        (spaces prec)
        .(showsHash h)
        .(showString " ")

        .(showString tag)
        .(showString "\n")
        .(showsPrec (prec + 1) l)
        .(secondChild)
        where
            tag
             | hasTwoChildren t = "-"
             | otherwise = "+"

            secondChild
             | hasTwoChildren t = (showsPrec (prec + 1) r)
             | otherwise = id

-- manage indent
spaces n = nfold n (showString "  ")

-- Composes some ShowS function n times with itself
nfold :: Int -> ShowS -> ShowS
nfold n ss
    | n == 0 = id
    | otherwise = ss.(nfold (n - 1) ss)

drawTree :: Show a => Tree a -> String
drawTree = show

