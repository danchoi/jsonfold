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
    directives :: [PathDirective]
  , debug :: Bool
  } deriving Show


parseOpts :: O.Parser Options
parseOpts = Options 
    <$> (parse . T.pack <$> O.argument O.str (O.metavar "DSL" <> O.help "Path directives DSL"))
    <*> O.flag False True (O.short 'd' <> O.long "debug" <> O.help "Debug directive parser. Does not parse STDIN")

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
  case debug of
    True -> print directives 
    False -> do
      s <- BL.getContents 
      let xs :: [Value]
          xs = decodeStream s
      forM_ xs $ \x -> 
          BL8.putStrLn . encode . reduceValue directives [] $ x
   
------------------------------------------------------------------------


reduceValue :: [PathDirective] -> Path -> Value -> Value
reduceValue ds ks (Object o) = Object $ HM.mapWithKey (\k v -> reduceValue ds (k:ks) v) o
-- CHANGEME Maybe not: --  Top level arrays should be reduced once. 
reduceValue ds ks v@(Array vs) = reduceArray ds ks v
-- all other values are scalar, should be copied directly
reduceValue _ _ v = v 

reduceArray :: [PathDirective] -> Path -> Value -> Value
reduceArray ds ks v@(Array _) = 
      foldr applyStrategy v ds'
    where ds' :: [Directive]
          ds' = let ws = concat [ys | (FullPathMatch ks', ys) <- ds, ks' == ks]
                in case ws of
                      [] -> concat [zs | (AnyPathFallBack, zs) <- ds]
                      _ -> ws


applyStrategy :: Directive -> Value -> Value

applyStrategy (Sort ord) (Array vs) = toArray $ orderOp ord $ sort $ V.toList vs 

applyStrategy (SortFreq ord) (Array vs) = 
    let xs :: [Value]
        xs = orderOp ord $ map head $ sortBy (compare `on` length) $ group $ sort $ V.toList vs
    in toArray xs

applyStrategy Nub (Array vs) = 
    let vs' = V.toList vs
    in toArray [v | v <- vs', v /= Null] 

applyStrategy Concat (Array vs) = 
    let vs' = V.toList vs
    in Array . V.fromList . concat $ [ V.toList v  | Array v <- vs' ] 

applyStrategy (ConcatSep delim) (Array vs) = 
    let vs' = V.toList vs
        vs'' = concat $ [ V.toList v  | Array v <- vs' ]  
        xs = intersperse delim $ map valToText vs''
    in String . mconcat $ xs

applyStrategy Compact (Array vs) = 
    let vs' = V.toList vs
    in toArray [ v | v <- vs', v /= Null ] 

applyStrategy Head (Array vs) =   
    let vs' = V.toList vs
    in case take 1 vs' of
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
    String x <= String y = T.length x <= T.length y || x <= y
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
-- | transform DSL

-- cast.actors( concat | head )
-- title( sortfreq.desc | head )
-- *( compact | sort.desc | head )  -- fallback
-- genres( sort.asc | compact | concatsep"," )



parse :: Text -> [PathDirective]
parse s = case AT.parseOnly pPathDirectives s of
    Left err -> error $ "Parse error " ++ err 
    Right res -> res

spaces = AT.many1 AT.space
spaces' = AT.many' AT.space

pPathDirectives :: AT.Parser [PathDirective]
pPathDirectives = pPathDirective `AT.sepBy` spaces

pPathDirective :: AT.Parser PathDirective
pPathDirective = do
    path <- pAnyPath <|> pFullPath
    spaces' >> AT.char '(' >> spaces'
    ds <- pDirective `AT.sepBy1` pipe
    spaces' >> AT.char ')' 
    return (path, ds)
    
pipe = spaces' >> AT.char '|' >> spaces' 

pAnyPath :: AT.Parser PathSpec
pAnyPath = AT.char '*' >> pure AnyPathFallBack

pFullPath = FullPathMatch <$> AT.sepBy1 pPathSeg (AT.takeWhile1 $ AT.inClass ".")

pPathSeg = AT.takeWhile1 (AT.notInClass " .[:")

pDirective :: AT.Parser Directive
pDirective = 
    AT.choice [
      AT.string "concat" >> pure Concat 
    , AT.string "nub" >> pure Nub 
    , AT.string "compcat" >> pure Compact
    , AT.string "head" >> pure Head
    , AT.string "concatsep" >> (ConcatSep <$> pSep)
    , AT.string "sort" >> (Sort <$> pOrder)
    , AT.string "sortfreq" >> (SortFreq <$> pOrder)
    -- sort, sortfreq
    ]

pSep = do
    spaces'
    openQuote <- AT.satisfy $ AT.inClass "\"'"
    sepString <- AT.takeTill (==openQuote) 
    -- close quote
    AT.char openQuote
    return sepString

pOrder = do
    AT.char '.' 
    (AT.string "asc" >> return Asc) <|> (AT.string "desc" >> return Desc) 



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




