{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards #-}
module Main where
import Data.Aeson
import Data.Monoid
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text.Encoding as T (decodeUtf8)
import Data.List (intersperse)
import qualified Data.List 
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Data.Maybe (catMaybes)
import Control.Applicative
import Control.Monad (when)
import qualified Data.ByteString.Lazy as BL hiding (map, intersperse)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Attoparsec.Lazy as Atto hiding (Result)
import Data.Attoparsec.ByteString.Char8 (endOfLine, sepBy)
import qualified Data.Attoparsec.Text as AT
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import Data.Scientific 
import System.Environment (getArgs)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder.RealFloat as B
import qualified Options.Applicative as O
import Data.List (foldl', foldl1', sort, nub)
import Data.Monoid

data Options = Options deriving Show

parseOpts :: O.Parser Options
parseOpts = pure Options 

opts = O.info (O.helper <*> parseOpts)
          (O.fullDesc 
            <> O.progDesc "Merge JSON objects"
            <> O.header "jsonmerge"
            <> O.footer "See https://github.com/danchoi/jsonfold for more information.")

data MergeValue = MergeObject (HashMap Text MergeValue)
                | MergeLeaf [Value] 
                deriving Show

data ReductionStrategy = Last 
        deriving Show

type  ReductionStrategies = [(Path, ReductionStrategy)]

data ReductionValue = 
        ReductionObject (HashMap Text ReductionValue)
      | ReductionLeaf [Value] Value ReductionStrategy
        deriving Show

main = do
  Options{..} <- O.execParser opts
  s <- BL.getContents 
  let xs :: [Value]
      xs = decodeStream s
      x :: MergeValue
      x = foldl' (mkMergeValue ) (MergeObject HM.empty) xs 
  -- print  x
  BL8.putStrLn . encode . debugReduce $ x
   
------------------------------------------------------------------------

mkMergeValue :: MergeValue -> Value -> MergeValue
mkMergeValue (MergeObject m) (Object v) = 
      MergeObject $ HM.mapWithKey f v 
    where f :: (Text -> Value -> MergeValue) 
          f k v' = mergeWithKey k m v'
mkMergeValue _ _ = error "Top-level Value must be an Object"

mergeWithKey :: Text -> HashMap Text MergeValue -> Value -> MergeValue
mergeWithKey k parentObj childObj@(Object _) 
      | Just c@(MergeObject _) <- HM.lookup k parentObj = mkMergeValue c childObj
      | otherwise = mkMergeValue (MergeObject HM.empty) childObj

mergeWithKey k o v  -- v is not an JSON object, could be null
      | Just (MergeLeaf vs) <- HM.lookup k o = MergeLeaf $ v:vs 
      | Just x <- HM.lookup k o = error $ "HM.lookup " ++ (T.unpack k) ++ " results in " ++ show x
      | otherwise                            = MergeLeaf [v]

------------------------------------------------------------------------

type Path = [Text]

reduceValue :: ReductionStrategies -> Path -> MergeValue -> ReductionValue
reduceValue rs ks (MergeObject o) = ReductionObject $ HM.mapWithKey (\k v -> reduceValue rs (k:ks) v) o
reduceValue rs ks (MergeLeaf vs) = 
    let r = undefined -- TODO select ReductionStrategy
    in reduceLeafValues r ks vs 

reduceLeafValues :: ReductionStrategy -> Path -> [Value] -> ReductionValue
reduceLeafValues r ks vs = undefined


-- | This just turns MergeLeaf into JSON arrays
debugReduce :: MergeValue -> Value 
debugReduce (MergeObject m) = Object . fmap debugReduce $ m
debugReduce (MergeLeaf vs) = Array . V.fromList $ vs

{-
unionWithKey :: (Text -> Value -> Value -> Value) -> HashMap Text Value -> HashMap Text Value -> HashMap Text Value
unionWithKey f o o' = 
      let o'MergedMatchingKeys = HM.mapWithKey f' o' 
          oUniqueKeys = HM.difference o o'
      in o'MergedMatchingKeys `HM.union` oUniqueKeys
    where f' :: Text -> Value -> Value
          f' key v | HM.member key o = mergeValue key v ((HM.!) o key)
                   | otherwise       = v
-}
{-
-- The first parameter is the accumulator
mergeValue :: Text -> Value -> Value -> Value
mergeValue key (Object v) (Object v') = 
    -- merge all the keys and recursively merged values
    Object $ unionWithKey mergeValue v' v 

mergeValue key (String v) (String v') = String (max v v')
mergeValue key (Bool v) (Bool v') = Bool (max v v')
mergeValue key (Number v) (Number v') = Number (max v v') -- ?
mergeValue key x Null = x
mergeValue key Null x = x
mergeValue key (Array v) (Array v') = if V.length v > V.length v' then Array v else Array v'
-- TODO we can merge the arrays and nub ? Won't work on version
-}


-- For reference: mapWithKey :: (k -> v1 -> v2) -> HashMap k v1 -> HashMap k v2

-- ^ Utility for deserialization

decodeStream :: (FromJSON a) => BL.ByteString -> [a]
decodeStream bs = case decodeWith json bs of
    (Just x, xs) | xs == mempty -> [x]
    (Just x, xs) -> x:(decodeStream xs)
    (Nothing, _) -> []

decodeWith :: (FromJSON a) => Parser Value -> BL.ByteString -> (Maybe a, BL.ByteString)
decodeWith p s =
    case Atto.parse p s of
      Atto.Done r v -> f v r
      Atto.Fail _ _ _ -> (Nothing, mempty)
  where f v' r = (\x -> case x of 
                      Success a -> (Just a, r)
                      _ -> (Nothing, r)) $ fromJSON v'



