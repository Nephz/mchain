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

type FreqMap = M.HashMap Tar Rational
type WalkMap = M.HashMap Src FreqMap

genChain :: (R.MonadRandom m) => WalkMap -> T.Text -> Iter -> m T.Text
genChain tm txt (cur, stop) =
  fmap (T.intercalate " ") $ genList tm txt (cur, stop)

genList :: (R.MonadRandom m) => WalkMap -> T.Text -> Iter -> m [T.Text]
genList tm txt (cur, stop) 
  | not (T.null txt) &&
    (cur == stop || DM.isNothing tar) = return [txt]
  | otherwise = 
    R.fromList (M.toList $ DM.fromJust tar) >>= \x ->
      genList tm x (cur+1, stop) >>= \xs ->
        return $ result txt xs
  where
    result :: T.Text -> [T.Text] -> [T.Text]
    result t ssl | T.null t = ssl
                 | otherwise = t : ssl
    tar = M.lookup txt tm  

-- "new" value is always only a singleton, so we flip the arguments in fold to get the evaluation: (let pls = f)
-- (f new old)
-- otherwise we would get a chain (as the old value could have multiple elements.)
-- (f old1 (f old2 (f old3 new)))
--            new        old
addWalks :: FreqMap -> FreqMap -> FreqMap
addWalks a b = M.foldrWithKey pls b a 
  where
    pls k v kvs =
      case M.lookup k kvs of
        Nothing -> M.insert k v kvs
        Just n  -> M.insert k (v+n) kvs

walkMap :: Walks -> WalkMap
walkMap = DL.foldl' insert M.empty
  where
    insert l (s, t) = M.insertWith addWalks s (M.singleton t 1) l

walks :: [T.Text] -> Walks
walks [] = []
walks (x:xs)  = zip ("" : wrds) wrds <> walks xs
  where 
    wrds = T.words $ T.replace "," " ," $ T.replace "." " ." x
