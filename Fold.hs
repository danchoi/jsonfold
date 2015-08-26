{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards #-}
module Main where
import Data.Aeson
import Data.Monoid (mempty, mconcat, (<>))
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text.Encoding as T (decodeUtf8)
import Data.List (intersperse)
import qualified Data.List 
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Data.Maybe (catMaybes)
import Control.Applicative
import Control.Monad (when, forM_)
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

data Options = Options { 
    keyPathToFold :: String
  } deriving Show


parseOpts :: O.Parser Options
parseOpts = pure $ Options ""

opts = O.info (O.helper <*> parseOpts)
          (O.fullDesc 
            <> O.progDesc "Fold merge JSON leaves with arrays"
            <> O.header "jsonfold"
            <> O.footer "See https://github.com/danchoi/jsonfold for more information.")


data Directive = 
        Sort Order -- Asc orders by lowest value first, meant for numbers and bool
                      -- For String, Array, Object, orders by shortest length (number of keys for obj)
      | SortFreq Order -- Desc orders by highest frequency first
      | Nub -- eliminate dupes, works like Haskell `nub`
      | Concat  -- concats array to arrays into one array
      | ConcatSep Text -- converts string, number, bool, null array elem to string and concats them with the delim value
      | Compact -- eliminate Nulls
      | Head -- takes 1st element of array or Null if empty
             -- Use this to de-dupe a list into one element, after sorting
             -- with the other directives above
    deriving (Eq, Show)

data Order = Asc | Desc deriving (Eq, Show)

data PathSpec = FullPathMatch [Text] | AnyPathFallBack  -- default
          deriving Show

type Path = [Text]

-- a path is like "versions.rental.price"
-- A key can be like "versions.rental.price"
-- default catchall path can be specified with "*" wildcard


-- Directives in order of application, left to right, like a unix pipeline

type PathDirective = (PathSpec, [Directive])

-- There should probably be a series of reduction strategies, one tried after another?
type ReductionStrategies = [(PathSpec, Directive)]


main = do
  Options{..} <- O.execParser opts
  s <- BL.getContents 
  let xs :: [Value]
      xs = decodeStream s
      rs = [ (FullPathMatch ["ASIN"], [Head]) ] 
  forM_ xs $ \x -> 
      BL8.putStrLn . encode . reduceValue rs [] $ x
   
------------------------------------------------------------------------


reduceValue :: [PathDirective] -> Path -> Value -> Value
reduceValue rs ks (Object o) = Object $ HM.mapWithKey (\k v -> reduceValue rs (k:ks) v) o
-- CHANGEME Top level arrays should be reduced once. 
reduceValue rs ks (Array vs) = reduceArray rs ks (V.toList vs)
-- all other values are scale, and should be copied directly
reduceValue _ _ v = v 

reduceArray :: [PathDirective] -> Path -> [Value] -> Value
reduceArray ds ks vs = foldr applyStrategy vs ds'
    where ds' :: [Directive]
          ds' = map snd $ filter (\d -> d `matchesPath` ks) ds
          matchesPath :: PathDirective -> Path -> Bool
          matchesPath (FullPathMatch path', _) path = path'== path
          matchesPath (AnyPathFallBack, _) _ = True

-- apply strategies until [Value] is a singleton list
-- TODO: NotNull should be a filter
-- Concat should take option delimiter;
-- ArrayConcat vs LeafConcat; LeafConcat can take a delimiter

applyStrategy :: Directive -> [Value] -> Value
-- may need to reverse these
applyStrategy (Sort ord) vs = toArray $ orderOp ord $ sort vs 
applyStrategy (SortFreq ord) vs = toArray $ orderOp ord $ sortBy (compare `on` length) $ group $ sort vs 
applyStrategy Nub vs = toArray [v | v <- vs, v /= Null] 

-- collapses Array of Arrays into one level of Array
applyStrategy Concat vs = Array . V.fromList . concat $ [ v  | Array v <- vs ] 

-- collapses Array of String, Bool, Number, or Null into a scalar string representation
applyStrategy (ConcatSep delim) vs = 
      let xs = intersperse delim $ map valToText $ applyStrategy Concat vs
      in String . mconcat $ xs

applyStrategy Compact vs = toArray [ v | v <- vs, v /= Null ] 
applyStrategy Head vs = case take 1 vs of
                          [] -> Null
                          [v] -> v

valToText :: Value -> Text
valToText (String x) = x
valToText Null = "NULL"
valToText (Bool True) = "TRUE"
valToText (Bool False) = "FALSE"
valToText (Number x) = 
    case floatingOrInteger x of
        Left float -> T.pack . show $ float
        Right int -> T.pack . show $ int
valToText (Array _) = "[Array]"
valToText (Object _) = "[Object]"


toArray :: [Value] -> Value
toArray = Array . V.fromList 

orderOp :: Order -> ([a] -> [a])
orderOp Asc = id
orderOp Desc = reverse

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
-- | KeyPath 

-- actors[concat]
-- actors[intersperse','|concat].name 
--         lifts name from objects first into an Array, then intersperses "," strings, then concats

data Key = Key Text | ArrayOp [Directive] deriving (Eq, Show)

parseKeyPath :: Text -> [PathDirective]
parseKeyPath s = case AT.parseOnly pPathDirectives s of
    Left err -> error $ "Parse error " ++ err 
    Right res -> res

spaces = AT.many1 AT.space

pPathDirectives :: AT.Parser [PathDirective]
-- pPathDirectives = pPathDirective `AT.sepBy` spaces
pPathDirectives = pure [Sort Asc]

pPathDirective :: AT.Parser PathDirective
pPathDirective = do
    path <- anyPath <|> (FullPathMatch <$> AT.sepBy1 pPathSeg (AT.takeWhile1 $ AT.inClass "."))
    ds <- pPathDirectives
    return (path, ds)
    
anyPath :: AT.Parser PathSpec
anyPath = AT.char '*' *> AnyPathFallBack

pPathSeg = Key <$> AT.takeWhile1 (AT.notInClass " .[:")

-- pDirective = pure (Sort Asc) 



------------------------------------------------------------------------

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




