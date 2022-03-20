-- hb417666
{-# LANGUAGE LambdaCase #-}

module HashTree (leaf, twig, node, buildTree, treeHash, drawTree, buildProof, merklePaths, verifyProof, showMerklePath, MerkleProof, MerklePath) where

import Data.Maybe (listToMaybe)
import Hashable32 (Hash, Hashable (hash), showHash)
import PPrint (intercalateS)

-- A
data BaseTree a = Leaf a | Node (Tree a) (Tree a) | Twig (Tree a)

type Tree a = (Hash, BaseTree a)

leaf :: Hashable a => a -> Tree a
leaf el = (hash el, Leaf el)

twig :: Hashable a => Tree a -> Tree a
twig (h, t) = (hash (h, h), Twig (h, t))

node :: Hashable a => Tree a -> Tree a -> Tree a
node (lh, lt) (rh, rt) = (hash (lh, rh), Node (lh, lt) (rh, rt))

buildTree :: Hashable a => [a] -> Tree a
buildTree lst =
  let tlst = map leaf (reverse lst)
   in buildTreeFromTrees tlst

treeHash :: Tree a -> Hash
treeHash = fst

drawTree :: Show a => Tree a -> String
drawTree t = drawTreeHelper (showString "") t ""

drawTreeHelper :: Show a => ShowS -> Tree a -> ShowS
drawTreeHelper prefix (h, t) =
  prefix . showString (showHash h) . showString " " . nodeSpecific
  where
    nodeSpecific =
      case t of
        Leaf l -> showString (show l) . showString "\n"
        Twig t -> showString "+\n" . drawChild t
        Node r l -> showString "-\n" . drawChild r . drawChild l
    drawChild = drawTreeHelper (prefix . showString "  ")

buildTreeFromTrees :: Hashable a => [Tree a] -> Tree a
buildTreeFromTrees [t] = t
buildTreeFromTrees lst = buildTreeFromTrees (reducePairs lst)

reducePairs :: Hashable a => [Tree a] -> [Tree a]
reducePairs lst = reducePairsHelper (reverse lst) []

reducePairsHelper :: Hashable a => [Tree a] -> [Tree a] -> [Tree a]
reducePairsHelper [] acc = acc
reducePairsHelper [a] acc = twig a : acc
reducePairsHelper (a : b : t) acc = reducePairsHelper t (node a b : acc)

-- B
type MerklePath = [Either Hash Hash]

data MerkleProof a = MerkleProof a MerklePath

instance Show a => Show (MerkleProof a) where
  showsPrec d (MerkleProof a path) =
    showParen (d > app_prec) $
      showString "MerkleProof " . showsPrec (app_prec + 1) a . showString " " . showString (showMerklePath path)
    where
      app_prec = 10

buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof el t =
  let paths = merklePaths el t
   in fmap (MerkleProof el) (listToMaybe paths)

merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths el t =
  reverse $ helper [] [([], t)]
  where
    hashEl = hash el
    helper res [] = res
    helper res ((p, (h, tree)) : t) =
      let (q, res') = case tree of
            Leaf _ -> (t, if h == hashEl then reverse p : res else res)
            Twig l@(lh, lt) -> ((Left lh : p, l) : t, res)
            Node l@(lh, lt) r@(rh, rt) ->
              ((Left rh : p, l) : (Right lh : p, r) : t, res)
       in helper res' q

verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof h (MerkleProof a p) =
  let calcH =
        foldr
          reducer
          (hash a)
          p
   in calcH == h
  where
    reducer (Left l) h = hash (h, l)
    reducer (Right r) h = hash (r, h)

showMerklePath :: MerklePath -> String
showMerklePath path =
  let mapped =
        map
          ( showString . \case
              Left l -> "<" ++ showHash l
              Right r -> ">" ++ showHash r
          )
          path
   in intercalateS (showString "") mapped ""