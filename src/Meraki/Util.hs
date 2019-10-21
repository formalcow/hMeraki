{-# LANGUAGE OverloadedStrings #-}

module Meraki.Util where

import GHC.Generics
import Control.Lens ((^.), (.~), (&))
import Control.Monad
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import Data.Either (fromRight)
import Data.Functor
import Data.Semigroup ((<>))
import Data.Scientific
import Data.Aeson
import System.IO
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Char8 as C
import Network.Wreq
import qualified Network.Wreq.Types as W
import Meraki.Types

-- Utility functions:
findDevSerial :: Serial -> [MerakiDevice] -> Maybe MerakiDevice
findDevSerial s (d:ds) = if s == deviceSerial d
                         then Just d
                         else findDevSerial s ds
findDevSerial _ _      = Nothing

findOrg :: String -> [MerakiOrg] -> Maybe MerakiOrg
findOrg i (o:os) = if orgId o == i
                           then Just o
                           else findOrg i os
findOrg _ _          = Nothing

findNet :: String -> [MerakiNetwork] -> Maybe MerakiNetwork
findNet i (n:ns) = if networkId n == i
                       then Just n
                       else findNet i ns
findNet _ _      = Nothing

isModel :: MerakiDevice -> MerakiModel -> Bool
isModel d m = (show m) `isPrefixOf` deviceModel d

-- Resources:
-- Files: local filesystem locations for API endpoint data
-- Meraki Dashboard API key, base URL, and options

resourceDir :: FilePath
resourceDir = "fs_resources/"

myAPIKeyFile :: FilePath
myAPIKeyFile = "keyFile.txt"

myAPIKey :: IO C.ByteString
myAPIKey = do
  key' <- C.readFile $ resourceDir <> myAPIKeyFile
  return $ C.filter (\c -> c /= '\n') key'

orgFile :: FilePath
orgFile = "hMeraki_org.txt"

netFile :: FilePath
netFile = "hMeraki_net.txt"

devFile :: FilePath
devFile = "hMeraki_dev.txt"

ssidFile :: FilePath
ssidFile = "hMeraki_ssid.txt"

-- Working environment org and network:
envOrgId :: IO String
envOrgId = readFile $ resourceDir <> orgFile

mkEnvOrg :: MerakiOrg -> IO ()
mkEnvOrg o = do
  h <- openFile (resourceDir <> orgFile) WriteMode
  hPutStr h (orgId o)
  hClose h

envNetId :: IO String
envNetId = readFile $ resourceDir <> netFile

mkEnvNet :: MerakiNetwork -> IO ()
mkEnvNet n = do
  h <- openFile (resourceDir <> netFile) WriteMode
  hPutStr h (networkId n)
  hClose h

baseURL  = "https://api.meraki.com/api/v0"

baseOpts :: IO W.Options
baseOpts = do
  myKey <- myAPIKey
  return $ defaults & header "X-Cisco-Meraki-API-Key" .~ [myKey]
                    & header "Content-Type" .~ ["application/json"]

merakiApiGet :: String -> IO (Response CL.ByteString)
merakiApiGet url = do
  opts <- baseOpts
  getWith opts $ baseURL <> url

merakiApiPost :: W.Postable p => String -> p -> IO (Response CL.ByteString)
merakiApiPost url params = do
  opts <- baseOpts
  postWith opts (baseURL <> url) params

-- response processor: extract response body, parse JSON for
-- a value (declared type a) and grab the Just value
-- unsafe/incomplete/partial because fromJust fails on Nothings
processAPI :: FromJSON a => Response CL.ByteString -> a
processAPI resp = fromJust . decode $ (resp ^. responseBody)

getMerakiOrg :: IO MerakiOrg
getMerakiOrg = do
  myOrgId <- envOrgId
  r' <- merakiApiGet $ "/organizations/" <> myOrgId
  return (processAPI r' :: MerakiOrg)

getMerakiNet :: IO MerakiNetwork
getMerakiNet = do
  myNetId <- envNetId
  r  <- merakiApiGet $ "/networks/" <> myNetId
  return (processAPI r :: MerakiNetwork)

getMerakiDevs :: IO [MerakiDevice]
getMerakiDevs = do
  myNetId <- envNetId
  r  <- merakiApiGet $ "/networks/" <> myNetId <> "/devices"
  return (processAPI r :: [MerakiDevice]) 

getMerakiSsids :: IO [MerakiSsid]
getMerakiSsids = do
  myNetId <- envNetId
  r  <- merakiApiGet $ "/networks/" <> myNetId <> "/ssids"
  return (processAPI r :: [MerakiSsid])
