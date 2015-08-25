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
parseOpts = pure Options 

opts = O.info (O.helper <*> parseOpts)
          (O.fullDesc 
            <> O.progDesc "Fold merge JSON leaves with arrays"
            <> O.header "jsonfold"
            <> O.footer "See https://github.com/danchoi/jsonfold for more information.")


-- If all values are equal (incl. all null) or there is only one value, then no reduction strategy is used. 

-- Max and Min compare length when applied to String, Array, or Object (number of keys)

data ReductionStrategy = 
       AllSame | Empty | First | Last | Max | Min | MinNotNull | Majority | Concat | NubConcat
    deriving Show

type Path = [Text]

-- There should probably be a series of reduction strategies, one tried after another?
type ReductionStrategies = [(Path, ReductionStrategy)]


main = do
  Options{..} <- O.execParser opts
  s <- BL.getContents 
  let xs :: [Value]
      xs = decodeStream s
      rs = [ Majority, First ]
  forM_ xs $ \x -> 
      BL8.putStrLn . encode . reduceValue rs [] $ x
   
------------------------------------------------------------------------


reduceValue :: [ReductionStrategy] -> Path -> Value -> Value
reduceValue rs ks (Object o) = Object $ HM.mapWithKey (\k v -> reduceValue rs (k:ks) v) o
-- Top level arrays should be reduced once. 
reduceValue rs ks (Array vs) = reduceArray rs ks (V.toList vs)
-- all other values are scale, and should be copied directly
reduceValue _ _ v = v 

reduceArray :: [ReductionStrategy] -> Path -> [Value] -> Value
reduceArray rs ks vs =
    let vs' = foldr applyStrategy vs (reverse rs)
    in head vs'

-- apply strategies until [Value] is a singleton list
-- TODO: NotNull should be a filter
-- Concat should take option delimiter;
-- ArrayConcat vs LeafConcat; LeafConcat can take a delimiter

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
-- These only work on Array of Array:
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
-- | KeyPath 

-- actors[concat]
-- actors[concat|intersperse','].name 
        lifts name from objects first into an Array, then intersperses "," strings, then concats
-- TODO rename ReductionStrategy to a generic operation

data Key = Key Text | ArrayOp [ReductionStrategy] deriving (Eq, Show)

parseKeyPath :: Text -> [KeyPath]
parseKeyPath s = case AT.parseOnly pKeyPaths s of
    Left err -> error $ "Parse error " ++ err 
    Right res -> res

spaces = many1 AT.space

pKeyPaths :: AT.Parser [KeyPath]
pKeyPaths = pKeyPath `AT.sepBy` spaces

pKeyPath :: AT.Parser KeyPath
pKeyPath = KeyPath 
    <$> (AT.sepBy1 pKeyOrIndex (AT.takeWhile1 $ AT.inClass ".["))
    <*> (pAlias <|> pure Nothing)

-- | A column header alias is designated by : followed by alphanum string after keypath
pAlias :: AT.Parser (Maybe Text)
pAlias = do
    AT.char ':'
    Just <$> AT.takeWhile1 (AT.inClass "a-zA-Z0-9_-")

pKeyOrIndex :: AT.Parser Key
pKeyOrIndex = pIndex <|> pKey

pKey = Key <$> AT.takeWhile1 (AT.notInClass " .[:")

pIndex = Index <$> AT.decimal <* AT.char ']'



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




