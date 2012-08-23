{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List (intercalate)

import System.Directory (getHomeDirectory)
import qualified System.FilePath as FP
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.Console.ANSI

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base64 as Base64

import Network.HTTP
import Network.URI

-- | loading the API config
data APIInfo = APIInfo { apiURL :: String, apiKey :: String} deriving (Show)

instance FromJSON APIInfo where
  parseJSON (Object v) = APIInfo <$> v .: "api-url" <*> v .: "api-key"
  parseJSON _ = mzero

loadAPIInfo :: IO (Maybe APIInfo) 
loadAPIInfo = do
  home <- getHomeDirectory
  src <- BL.readFile $ home `FP.combine` ".multivac"
  return $ decode src

-- | json parsing for items
data Entry = Entry
  { entryID :: String
  , entryBody :: Maybe String
  , entryTags :: [String]
  , entryDate :: String
  , entryLink :: Maybe String
  } deriving (Eq, Show)

instance FromJSON Entry where
  parseJSON (Object v) = Entry <$>
                         ((v .: "_id") >>= (.: "$oid")) <*>
                         v .:? "body" <*>
                         v .: "tags" <*>
                         ((v .: "ts") >>= (.: "$date")) <*>
                         v .:? "link"
  parseJSON _ = mzero


-- | cmdline option parsing
data Option = Help | Delete | Add deriving (Eq, Show)

options :: [OptDescr (Option, String)]
options =
  [ Option ""  ["help"] (NoArg (Help, "")) "display this help"
  , Option "d" ["delete"] (ReqArg ((,) Delete) "STRING") "delete an entry by id"
  , Option "a" ["add"] (ReqArg ((,) Add) "STRING") "add an entry"
  ]

header :: String
header = "Usage: multivac [global-options] command [command-options]\n\n\
         \Available global-options are:\n"

usageerror :: String -> IO ()
usageerror errormsg = do
  putStrLn errormsg
  putStrLn (usageInfo header options)
  exitFailure

-- | construct a Basic authorization header from an APIInfo key string
mkAuthHeader :: String -> Header
mkAuthHeader un = mkHeader HdrAuthorization b64str
  where b64str = "Basic " ++ B.unpack (Base64.encode $ B.pack $ un ++ ":X")

mkSearchRequest :: APIInfo -> [String] -> Request BL.ByteString
mkSearchRequest api tags = Request
  { rqURI = fromJust $ parseURI (apiURL api ++ "search/" ++ intercalate "," tags)
  , rqMethod = GET
  , rqHeaders = [mkAuthHeader $ apiKey api]
  , rqBody = ""
  }

main ::  IO ()
main = do
  argv <- getArgs
  case getOpt RequireOrder options argv of
    (o, n, []) -> worker o n
    (_, _, errors) -> usageerror (concat errors) 

worker :: [(Option, String)] -> [String] -> IO ()
worker args commandargs = do
  when (isJust $ lookup Help args) $ usageerror ""
  {-when (isJust $ lookup Delete args) $ print "delete"-}
  case commandargs of
    (_:_) -> doSearch commandargs
    _     -> usageerror "" 

-- | color helpers
rc :: IO ()
rc = setSGR [Reset]

sc :: ColorIntensity -> Color -> IO ()
sc i c = setSGR [SetColor Foreground i c]

-- | outputting entries
outputEntry :: Entry -> IO ()
outputEntry e = do
  sc Dull Green
  putStr $ entryDate e ++ " "
  sc Vivid Green
  putStr $ entryID e ++ " "
  sc Dull Cyan
  putStr $ "(" ++ intercalate ", " (entryTags e) ++ ") "
  sc Vivid Blue
  case entryBody e of
    Just x -> putStr $ "\n" ++ x
    Nothing -> return ()
  case entryLink e of
    Just x -> putStrLn $ "\nLink: " ++ x ++ "\n"
    Nothing -> putStrLn "\n"

doSearch :: [String] -> IO ()
doSearch tags = do
  api <- loadAPIInfo
  let req = mkSearchRequest (fromJust api) tags
  body <- simpleHTTP req >>= getResponseBody
  let entries = decode body :: Maybe [Entry]
  mapM_ outputEntry $ fromJust entries
  rc
