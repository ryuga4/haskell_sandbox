module Lib where

import Control.Monad
import Control.Monad.CC
import Control.Monad.Writer.Strict
import Control.Monad.IO.Class
import Data.Monoid
import Control.Monad.Cont

someFunc :: IO ()
someFunc = putStrLn "someFunc"



data Tree a = Leaf a
            | Node a (Tree a) (Tree a)
            deriving (Show)


t :: Tree Int
t = Node 1 (Node 2 (Leaf 3)
                   (Node 4  (Leaf 5)
                            (Leaf 6)))
           (Node 7 (Node 8  (Leaf 9)
                            (Leaf 10))
                   (Leaf 11))

toList (Leaf i) = [i]
toList (Node a t1 t2) = a : toList t1 ++ toList t2



visit :: MonadDelimitedCont p s m => (a -> m b) -> p b -> Tree a -> m b
visit f p = visit'
 where
 visit' (Leaf i)       = control p $ \k -> k (f i)
 visit' (Node i t1 t2) = control p $ \k -> do 
                                            a <- k (f i)
                                            visit' t1
                                            visit' t2
                                            return a

bf :: MonadDelimitedCont p s m => (a -> m b) -> Tree a -> m b
bf f t = reset $ \p -> visit f p t


runBF :: Monad m => (a -> m b) -> Tree a -> m b
runBF f t = runCCT (bf (lift . f) t)


printTree :: Tree Int -> IO ()
printTree t = flip runContT (return) $ do 
    a <- callCC $ \k -> do
        runBF (\i -> when (i==8) (k ()) >> (lift . print) i) t
        return ()
    return ()


collectTree :: Tree Int -> [Int]
collectTree t = snd $ runWriter $ flip runContT (tell) $ do 
    callCC $ \k -> [] <$ runBF (\i -> when (i==18) (k []) >> (lift . tell) [i]) t
    

