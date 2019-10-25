{-# LANGUAGE OverloadedStrings #-}

module Meraki.Util where

import Control.Lens ((^.), (.~), (&))
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

camFile :: FilePath
camFile = "hMeraki_cam.txt"

ssidFile :: FilePath
ssidFile = "hMeraki_ssid.txt"

-- Working environment org and network:
envOrgId :: IO Serial
envOrgId = readFile $ resourceDir <> orgFile

mkEnvOrg :: MerakiOrg -> IO ()
mkEnvOrg o = do
  h <- openFile (resourceDir <> orgFile) WriteMode
  hPutStr h (orgId o)
  hClose h

envNetId :: IO Serial
envNetId = readFile $ resourceDir <> netFile

mkEnvNet :: MerakiNetwork -> IO ()
mkEnvNet n = do
  h <- openFile (resourceDir <> netFile) WriteMode
  hPutStr h (networkId n)
  hClose h

envCamId :: IO Serial
envCamId = readFile $ resourceDir <> camFile

mkEnvCam :: MerakiDevice -> IO ()
mkEnvCam c = do
  h <- openFile (resourceDir <> camFile) WriteMode
  hPutStr h (deviceSerial c)
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

getOrg :: IO MerakiOrg
getOrg = do
  myOrgId <- envOrgId
  r' <- merakiApiGet $ "/organizations/" <> myOrgId
  return (processAPI r' :: MerakiOrg)

getNet :: IO MerakiNetwork
getNet = do
  myNetId <- envNetId
  r'  <- merakiApiGet $ "/networks/" <> myNetId
  return (processAPI r' :: MerakiNetwork)

getDevs :: IO [MerakiDevice]
getDevs = do
  myNetId <- envNetId
  r'  <- merakiApiGet $ "/networks/" <> myNetId <> "/devices"
  return (processAPI r' :: [MerakiDevice]) 

getSsids :: IO [MerakiSsid]
getSsids = do
  myNetId <- envNetId
  r'  <- merakiApiGet $ "/networks/" <> myNetId <> "/ssids"
  return (processAPI r' :: [MerakiSsid])

getMvs :: IO [MerakiDevice]
getMvs = (filter (flip isModel MV)) <$> getDevs

getMvLive :: Serial -> IO MerakiSense
getMvLive s = do
  r' <- merakiApiGet $ "/devices/" <> s <> "/camera/analytics/live"
  return (processAPI r' :: MerakiSense)


getMvZones :: Serial -> IO [MerakiCameraZone]
getMvZones s = do
  r' <- merakiApiGet $ "/devices/" <> s <> "/camera/analytics/zones"
  return (processAPI r' :: [MerakiCameraZone])
