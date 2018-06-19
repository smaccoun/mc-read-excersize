{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Csv
import Data.Char
import qualified Data.List as L (sortOn, foldl)
import qualified Data.Vector as V
import Debug.Trace
import qualified Data.Map.Strict as M


{-
 Return result of data considered relevant book info
 Can use any values from BookEntity
-}
data BookInfo =
  BookInfo
    {numRecommendations :: Int
    ,recommender :: String
    ,source      :: String
    ,amazonLink  :: String
    } deriving (Show)

{-
  An index that represents a single book entity
-}
data BookIndex =
  BookIndex
    {title'  :: String
    ,author' :: String
    } deriving (Eq, Show, Ord)

{-
 Ingested data row type
-}
data BookEntity =
  BookEntity
    {title     :: String
    ,author :: String
    ,_recommender :: String
    ,_source      :: String
    ,_amazon_link :: String
    ,description :: String
    ,type'       :: String
    ,genre     :: String
    , length :: String
    ,publish_year :: String
    ,on_list      :: String
    ,review_excerpt  :: String
    } deriving (Show)

instance FromNamedRecord BookEntity where
    parseNamedRecord r = BookEntity
       <$> r .: "Title"
       <*> r .: "Author"
       <*> r .: "Recommender"
       <*> r .: "Source"
       <*> r .: "Amazon_Link"
       <*> r .: "Description"
       <*> r .: "Type"
       <*> r .: "Genre"
       <*> r .: "Length"
       <*> r .: "Publish_Year"
       <*> r .: "On_List"
       <*> r .: "Review_Excerpt"


main :: IO ()
main = do
  eitherRes <- getNumberRecommendations
  case eitherRes of
    Left err -> putStrLn err
    Right v -> do
      _ <- putStrLn "Gather data and Writing to File...\n"
      writeBookRecommendationInfo v
      _ <- putStrLn $ "Done. See results.txt for result"
      return ()

writeBookRecommendationInfo :: M.Map BookIndex BookInfo -> IO ()
writeBookRecommendationInfo v = do
      let metaText = "Num Books: " ++ (show $ M.size v)
          sortedByRec = L.sortOn (numRecommendations . snd) (M.toList v)
      let fullTextInfo = L.foldl showBook "" (sortedByRec)
          fullText = metaText ++ "\n\n\n*********************\n\n\n" ++ fullTextInfo
      writeFile "results.txt" fullText
      return ()
    where
      showBook curStr (k, b) = curStr ++ "\n--------------------------\n" ++ show k ++ "\n" ++ show b


getNumberRecommendations :: IO (Either String (M.Map BookIndex BookInfo))
getNumberRecommendations = do
  csvData <- BL.readFile "master_list"
  let decOptions = defaultDecodeOptions {decDelimiter = fromIntegral $ ord '\t' }
  case decodeByNameWith decOptions csvData of
        Left err -> return $ Left err
        Right (_, v) -> do
          return $ Right $ V.foldl  mkRecommendationMap M.empty v

mkRecommendationMap :: M.Map BookIndex BookInfo -> BookEntity -> M.Map BookIndex BookInfo
mkRecommendationMap curMap curRow =
  case M.lookup curKey curMap of
    Just v -> M.insert curKey (v {numRecommendations = (numRecommendations v + 1)}) curMap
    Nothing -> M.insert curKey (initialBookInfo curRow) curMap
  where
    curKey = (getBookMapKey curRow)


getBookMapKey :: BookEntity -> BookIndex
getBookMapKey rowValue =
  (BookIndex (title rowValue) (author rowValue))

initialBookInfo :: BookEntity -> BookInfo
initialBookInfo rowValue =
    BookInfo
      {numRecommendations = 1
      ,recommender = _recommender rowValue
      ,source      = _source rowValue
      ,amazonLink  = _amazon_link rowValue
      }


