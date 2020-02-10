{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( readData, deleteData, addSchema, updateData, insertData
    ) where
import qualified Data.ByteString.Lazy as B

import Data.Aeson 
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import GHC.Exts
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Vector as V
import Data.Aeson.Text (encodeToLazyText)
import System.IO.Unsafe
import Text.Read
import qualified Data.Text.Encoding as TS
import Data.ByteString.Lazy.Char8 as Char8 hiding (putStrLn, getLine, filter, map, foldl, takeWhile, dropWhile)
import Data.Text.Lazy.IO as I hiding (putStrLn, getLine, filter, map, foldl, takeWhile, dropWhile)

removeChar :: Char -> String -> String
removeChar _ "" = ""
removeChar ch (x:xs)
    | x == ch = xs
    | otherwise = x:xs

path' :: String -> [String]
path' "" = []
path' xs = takeWhile (/= '/') xs : path' (removeChar '/' (dropWhile (/='/') xs))

path :: T.Text -> [T.Text]
path str = map T.pack (path' (T.unpack str))

getChild :: Value -> T.Text -> Value
getChild (Object obj) field = fromJust (HM.lookup field obj)

fromArrayToList :: Value -> [Value]
fromArrayToList(Array arr) = V.toList arr

readKeys :: Value -> [T.Text]
readKeys (Object x) = HM.keys x

readValue :: Value -> [T.Text] -> Value
readValue = foldl getChild

-- universal setter with path
writeValue :: Value -> [T.Text] -> Value -> Value
writeValue _ [] value = value
writeValue obj [field] value = setProperty obj field value
writeValue obj (x:xs) value = Object $ fromList ((x, (writeValue (getChild obj x) xs value)) : (filter (\(p, q) -> p/=x) (objToList obj)))

toArray :: [Value] -> Value
toArray xs = Array (V.fromList xs)

fromJust :: Maybe Value -> Value
fromJust (Just obj) = obj

objToList :: Value -> [(T.Text,Value)]
objToList (Object o) = HM.toList o

-- setter for first level properties
setProperty :: Value -> T.Text -> Value -> Value
setProperty (Object obj) field value = Object (HM.insert field value obj) 

filePath :: FilePath
filePath = "db.json"

readJSON :: FilePath -> IO B.ByteString
readJSON = B.readFile

getJSON :: IO B.ByteString -> B.ByteString
getJSON = unsafePerformIO


selectSchema = do
  putStrLn "Select schema"
  schema <- getLine
  return ("db/" ++ schema) 

objectAt :: [Value] -> Int -> Value
objectAt (x:xs) 0 = x
objectAt (x:xs) n = objectAt xs (n - 1)

printObjects :: [Value] -> Maybe Int -> IO ()
printObjects xs Nothing = B.putStrLn (encode (toArray xs))
printObjects xs (Just n) = B.putStrLn (encode (objectAt xs n))

--without where clause
readData = do 
  schema <- selectSchema
  putStrLn "Select properties (xpath like)"
  props <- getLine
  putStrLn "Index?"
  indexString <- getLine 
  let currentJson = getJSON (readJSON filePath)
  encodedJSON <- case (decode currentJson :: Maybe Value) of
    Just x  -> return x
    Nothing -> fail "expected valid json"
  let listOfElements = fromArrayToList $ readValue encodedJSON (path (T.pack schema))
  let result = map (\x -> readValue x (path (T.pack props))) listOfElements
  let index = readMaybe indexString :: Maybe Int
  printObjects result index
  return 0

deleteData = do
  schema <- selectSchema
  putStrLn "Select property for where statement (xpath like syntax)"
  props <- getLine
  putStrLn "Set value for property"
  rawJson <- getLine
  let currentJson = getJSON (readJSON filePath)
  encodedJSON <- case (decode currentJson :: Maybe Value) of
    Just x  -> return x
    Nothing -> fail "expected valid json"
  encodedValue <- case (decode (Char8.pack rawJson):: Maybe Value) of
    Just x -> return x
    Nothing -> fail "expected valid json"
  let listOfElements = fromArrayToList $ readValue encodedJSON (path (T.pack schema))
  let result = filter (\x -> readValue x (path (T.pack props)) /= encodedValue) listOfElements
  writeToDb (writeValue encodedJSON (path (T.pack schema)) (toArray result))
  putStrLn "Operation executed successfully"
  return 0

updateData = do
  schema <- selectSchema
  putStrLn "Select property for where statement (xpath like syntax)"
  props <- getLine
  putStrLn "Set value for property"
  rawJson <- getLine
  putStrLn "Select property for updating (xpath like syntax)"
  propsForUpdate <- getLine
  putStrLn "Set value for property"
  rawUpdateJson <- getLine
  let currentJson = getJSON (readJSON filePath)
  encodedJSON <- case (decode currentJson :: Maybe Value) of
    Just x  -> return x
    Nothing -> fail "expected valid json"
  encodedValue <- case (decode (Char8.pack rawJson):: Maybe Value) of
    Just x -> return x
    Nothing -> fail "expected valid json"
  encodedUpdateValue <- case (decode (Char8.pack rawUpdateJson) :: Maybe Value) of
    Just x -> return x
    Nothing -> fail "expected valid json"
  let listOfElements = fromArrayToList $ readValue encodedJSON (path (T.pack schema))
  let notSelectedEelements = filter (\x -> readValue x (path (T.pack props)) /= encodedValue) listOfElements
  let selectedElements = filter (\x -> readValue x (path (T.pack props)) == encodedValue) listOfElements
  let updatedData = map (\x -> writeValue x (path (T.pack propsForUpdate)) encodedUpdateValue)  selectedElements 
  writeToDb (writeValue encodedJSON (path (T.pack schema))(toArray (notSelectedEelements ++ updatedData)))
  putStrLn "Operation executed successfully"  
  return 0

addSchema = do 
  schema <- selectSchema
  let currentJson = getJSON (readJSON filePath)
  encodedJSON <- case (decode currentJson :: Maybe Value) of
    Just x  -> return x
    Nothing -> fail "expected valid json"
  writeToDb (writeValue encodedJSON (path (T.pack schema)) (toArray []))
  putStrLn "Operation executed successfully"  
  return 0

writeToDb :: Value -> IO ()
writeToDb value = I.writeFile filePath (encodeToLazyText value)


insertData = do
  schema <- selectSchema
  putStrLn "Enter object in json notation"
  rawJson <- getLine
  let currentJson = getJSON (readJSON filePath)
  encodedJSON <- case (decode currentJson :: Maybe Value) of
    Just x  -> return x
    Nothing -> fail "expected valid json"
  encodedValue <- case (decode (Char8.pack rawJson):: Maybe Value) of
    Just x -> return x
    Nothing -> fail "expected valid json"
  let listOfElements = fromArrayToList $ readValue encodedJSON (path (T.pack schema))
  writeToDb (writeValue encodedJSON (path (T.pack schema))(toArray (encodedValue:listOfElements)))
  putStrLn "Operation executed successfully"
  return 0