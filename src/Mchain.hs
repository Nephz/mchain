{-# LANGUAGE OverloadedStrings #-}

module Mchain where

import qualified Control.Monad.Random as R
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Maybe as DM
import qualified Data.List as DL 

type Src = T.Text
type Tar = T.Text
type Iter = (Integer, Integer)

type Walks = [(Src, Tar)]
type TarFreq = (Tar, Rational)
type WalkMap = M.HashMap Src [TarFreq]

genChain :: (R.MonadRandom m) => WalkMap -> T.Text -> Iter -> m T.Text
genChain tm txt (cur, stop) =
  fmap (T.intercalate " ") $ genList tm txt (cur, stop)

genList :: (R.MonadRandom m) => WalkMap -> T.Text -> Iter -> m [T.Text]
genList tm txt (cur, stop) 
  | not (T.null txt) &&
    (cur == stop || DM.isNothing tar) = return [txt]
  | otherwise = 
    R.fromList (DM.fromJust tar) >>= \x ->
      genList tm x (cur+1, stop) >>= \xs ->
        return $ result txt xs
  where
    result :: T.Text -> [T.Text] -> [T.Text]
    result t ssl | T.null t = ssl
                 | otherwise = t : ssl
    tar = M.lookup txt tm  

--            new          old
addWalk :: [TarFreq] -> [TarFreq] -> [TarFreq]
addWalk [(t, f)] ts = 
  case maybeKey t ts of
    Nothing -> (t,  f ) : ts
    Just n  -> (t, n+f) : filter (\(x, _) -> (x /= t)) ts
addWalk _ ts = ts 

maybeKey :: (Eq x) => x -> [(x, y)] -> Maybe y
maybeKey _ [] = Nothing
maybeKey k ((x,y):xys) | k == x    = Just y
                       | otherwise = maybeKey k xys

walkMap :: Walks -> WalkMap
walkMap = DL.foldl' insert M.empty
  where
    insert l (s, t) = M.insertWith addWalk s [(t, 1)] l

-- walkMap :: Walks -> WalkMap
-- walkMap = DL.foldr insert M.empty
--   where
--     insert (s, t) = M.insertWith addWalk s [(t, 1)]

walks :: [T.Text] -> Walks
walks [] = []
walks (x:xs)  = zip ("" : wrds) wrds <> walks xs
  where 
    wrds = T.words $ T.replace "," " ," $ T.replace "." " ." x
