{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards #-}
module Main where
import Data.Aeson
import Data.Monoid (mempty, (<>))
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
import qualified Data.Attoparsec.Lazy as Atto hiding (Result)
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
import Data.List (foldl', foldl1', sort, nub, maximum, minimum, sortBy, group)
import Data.Function (on)
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

-- If all values are equal (incl. all null) or there is only one value, then no reduction strategy is used. 

-- Max and Min compare length when applied to String, Array, or Object (number of keys)

data ReductionStrategy = 
       AllSame | Empty | First | Last | Max | Min | MinNotNull | Majority | Concat | NubConcat
    deriving Show

type Path = [Text]

-- There should probably be a series of reduction strategies, one tried after another?
type ReductionStrategies = [(Path, ReductionStrategy)]

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
      | otherwise                                       = mkMergeValue (MergeObject HM.empty) childObj
mergeWithKey k o v  -- v is not an JSON object, could be null
      | Just (MergeLeaf vs) <- HM.lookup k o = MergeLeaf $ v:vs 
      | Just x <- HM.lookup k o              = error $ "HM.lookup " ++ (T.unpack k) ++ " results in " ++ show x
      | otherwise                            = MergeLeaf [v]

------------------------------------------------------------------------

reduceValue :: ReductionStrategies -> Path -> MergeValue -> ReductionValue
reduceValue rs ks (MergeObject o) = ReductionObject $ HM.mapWithKey (\k v -> reduceValue rs (k:ks) v) o
reduceValue rs ks (MergeLeaf vs) = 
    let r = undefined -- TODO select ReductionStrategy
    in reduceLeafValues r ks vs 

reduceLeafValues :: ReductionStrategy -> Path -> [Value] -> ReductionValue
reduceLeafValues r ks vs 
    -- TODO
    | length (nub vs) == 1 = ReductionLeaf vs (head $ nub vs) AllSame
    | null vs              = ReductionLeaf vs Null Empty
    | otherwise = ReductionLeaf vs v r
        where v = Null -- CHANGEME

-- apply strategies until [Value] is a singleton list

applyStrategy :: ReductionStrategy -> [Value] -> [Value]
applyStrategy AllSame vs = nub vs 
applyStrategy Empty   vs = if null vs then [Null] else vs
-- may need to reverse these
applyStrategy First   vs = take 1 vs 
applyStrategy Last    vs = take 1 $ reverse vs
applyStrategy Max     vs = [ maximum vs ]
applyStrategy Min     vs = [ minimum vs ]
applyStrategy MinNotNull vs = [ minimum [v | v <- vs, v /= Null] ]
applyStrategy Majority vs = 
      let vs' = reverse $ sortBy (compare `on` length) $ group $ sort vs 
      in take 1 . concat $ vs'
applyStrategy Concat vs = [ Array . V.fromList . concat $ [ V.toList v  | Array v <- vs ] ]
applyStrategy NubConcat vs = [ Array . V.fromList . nub . concat $ [ V.toList v  | Array v <- vs ] ]

-- class  (Eq a) => Ord a  where
--    (<), (<=), (>=), (>)  :: a -> a -> Bool
--    max, min              :: a -> a -> a

instance Ord Value where
    String x <= String y = T.length x <= T.length y
    Number x <= Number y = x <= y
    Bool x   <= Bool y   = x <= y
    Null     <= _        = True
    Array xs <= Array ys = V.length xs <= V.length ys
    Object xs <= Object ys = 
        let x = length . HM.keys $ xs 
            y = length . HM.keys $ ys 
        in x <= y  -- just need something that doesn't blow up
    x <= y = error $ "Can't compare unlike Value types: " ++ show (x,y)

------------------------------------------------------------------------


-- | This just turns MergeLeaf into JSON arrays
debugReduce :: MergeValue -> Value 
debugReduce (MergeObject m) = Object . fmap debugReduce $ m
debugReduce (MergeLeaf vs) = Array . V.fromList $ vs



-- For reference: mapWithKey :: (k -> v1 -> v2) -> HashMap k v1 -> HashMap k v2

-- ^ Utility for deserialization

decodeStream :: (FromJSON a) => BL.ByteString -> [a]
decodeStream bs = case decodeWith json bs of
    (Just x, xs) | xs == mempty -> [x]
    (Just x, xs) -> x:(decodeStream xs)
    (Nothing, _) -> []

decodeWith :: (FromJSON a) => Atto.Parser Value -> BL.ByteString -> (Maybe a, BL.ByteString)
decodeWith p s =
    case Atto.parse p s of
      Atto.Done r v -> f v r
      Atto.Fail _ _ _ -> (Nothing, mempty)
  where f v' r = (\x -> case x of 
                      Success a -> (Just a, r)
                      _ -> (Nothing, r)) $ fromJSON v'



