module TreeByLevels where

data TreeNode a = TreeNode {
  left  :: Maybe (TreeNode a),
  right :: Maybe (TreeNode a),
  value :: a
} deriving Show

treeByLevels :: Maybe (TreeNode a) -> [a]
treeByLevels Nothing = []
treeByLevels (Just t) = traverse [t]
  where traverse [] = []
        traverse xs = map value xs ++ traverse (concatMap f xs)
          where f (TreeNode Nothing Nothing _)   = []
                f (TreeNode (Just l) Nothing _)  = [l]
                f (TreeNode Nothing (Just r) _)  = [r]
                f (TreeNode (Just l) (Just r) _) = [l, r]
