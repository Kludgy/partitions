-- |
module Data.Partitions
  (
  stirling2,
  partitions
  ) where

import Control.Monad.Writer

-- | Generation of Stirling Numbers of the Second Kind is used as the basis for
-- inducing set partitions on a list.
--
-- The implementation for 'stirling2' is a transcription of the algorithm described here:
-- <https://blogs.msdn.microsoft.com/oldnewthing/20140324-00/?p=1413 https://blogs.msdn.microsoft.com/oldnewthing/20140324-00/?p=1413>
--
-- Concatentation effects are captured with "Control.Monad.Writer".
--
stirling2 :: Int     -- ^ Partition block size 'k'.
          -> [a]     -- ^ Input set 'ns'.
          -> [[[a]]] -- ^ Partitions of 'ns' satisfying 'k'.

stirling2 k ns = execWriter (go k ns write) where
  go 0     []   f = f []
  go _     []   _ = pure ()
  go 0     _    _ = pure ()
  go k' (n:ns') f = do
    go (k'-1) ns' $ \xss ->       f $ [n] : xss
    go  k'    ns' $ \xss -> mapM_ f $ mapDiag (n:) xss k' -- An interesting sort of diagonal transformation is extracted.

  write x = writer ((), pure x)
    
-- | Map only the diagonal of the matrix whose rows 'xs' are replicated n times.
--
mapDiag :: (a -> a) -> [a] -> Int -> [[a]]
mapDiag f xs n = go n where
  go 0 = []
  go i = let (ys,zs) = splitAt (n-i) xs in
         (if null zs then ys else ys ++ f (head zs) : tail zs) : go (i-1)

-- | Induced set partitions of 'xs' produced by successive calls to 'stirling2'
-- with increasing block size.
--
partitions :: [a]     -- ^ Input set 'xs'.
           -> [[[a]]] -- ^ Partitions of 'xs'.

partitions xs = concat (go 1 xs) where
  go _ [] = []
  go n (_:ys) = stirling2 n xs : go (n+1) ys
