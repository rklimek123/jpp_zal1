-- rk418291
module HashTree where

import Hashable32
import Utils


-- { MY FUNCTIONS } --


-- returns True if the Tree has two children, false otherwise
hasTwoChildren :: Tree a -> Bool
hasTwoChildren (Node _ l r) =
    treeHash l /= treeHash r
hasTwoChildren (Leaf _ _) = False


-- manage indent
spaces n = nfold n (showString "  ")

-- Composes some ShowS function n times with itself
nfold :: Int -> ShowS -> ShowS
nfold n ss
    | n == 0 = id
    | otherwise = ss.(nfold (n - 1) ss)


-- { MY FUNCTIONS END } --



-- { Part A } --


data Tree a =
    Leaf Hash a
    | Node Hash (Tree a) (Tree a)


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
        aux (x:y:xs) _ =
            (node x y):(aux xs True)
        
        -- The second parameter indicates,
        -- whether the call to aux is recursive.
        -- It then should treat the element as
        -- a last element in list, therefore duplicating it.
        -- Otherwise, it should treat it as the only element
        -- in the list, which makes it the root.
        aux [x] recursive
            | recursive = [twig x]
            | otherwise = [x]
        aux [] _ = []
        
        rec_aux l
            | length res == 1 = res
            | otherwise = rec_aux res
          where
              res = aux l False


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

drawTree :: Show a => Tree a -> String
drawTree = show


-- { Part A End } --



-- { Part B } --


type MerklePath = [Either Hash Hash]
data MerkleProof a = MerkleProof a MerklePath


instance Show a => Show (MerkleProof a) where
    showsPrec _ (MerkleProof x p) =
        (showString "MerkleProof ")
        .(showParen True $ shows x)
        .(showString " ")
        .(showString.showMerklePath $ p)


buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof a (Leaf h _)
    | hash a == h = Just (MerkleProof a [])
    | otherwise = Nothing

buildProof a (Node _ l r) =
    case buildProof a l of
        Just mp@(MerkleProof x p) ->
            Just (appendToProof mp $ Left (treeHash r))
        Nothing ->

            case buildProof a r of
                Just mp@(MerkleProof x p) ->
                    Just (appendToProof mp $ Right (treeHash l))
                Nothing ->
                    Nothing
    where
        -- appends Either to path inside MerkleProof
        appendToProof :: MerkleProof a -> Either Hash Hash -> MerkleProof a
        appendToProof (MerkleProof x p) e =
            MerkleProof x (e:p)


merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths a (Node _ l r) =
    paths_to_left ++ paths_to_right
    where
        paths_to_left = map ((Left $ treeHash r):) $ merklePaths a l
        paths_to_right = map ((Right $ treeHash l):) $ merklePaths a r

merklePaths a (Leaf h _)
    | hash a == h = [[]]
    | otherwise = []

showMerklePath :: MerklePath -> String
showMerklePath (x:xs) =
    ((showString dir).(showsHash $ fromEither x)) (showMerklePath xs)
    where
        dir =
            case x of
                Left _ -> "<"
                Right _ -> ">"

showMerklePath [] = ""


verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof h (MerkleProof a p) =
    h == cumHash (hash a) p
    where
        -- cumulative Hash reconstructs the root of the proof-tree
        cumHash :: Hash -> MerklePath -> Hash
        cumHash hh (x:xs) =
            pairOfEither x
            where
                h2 = cumHash hh xs
                pairOfEither =
                    either pairHashL pairHashR
                pairHashL = pairHashGen h2
                pairHashR = (flip pairHashGen) h2
                pairHashGen hx xx = hash (hx, xx)
        cumHash hh [] = hh


-- { Part B End } --
