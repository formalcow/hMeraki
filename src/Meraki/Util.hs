{-# LANGUAGE OverloadedStrings #-}

module Meraki.Util where

import GHC.Generics
import Control.Lens
import Control.Monad
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import Data.Either (fromRight)
import Data.Functor
import Data.Semigroup ((<>))
import Data.Scientific
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Encode.Pretty
import System.IO
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Char8 as C
import Network.Wreq
import Network.Wreq.Types
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

isMV :: MerakiDevice -> Bool
isMV d = "MV" `isPrefixOf` deviceModel d

isMR :: MerakiDevice -> Bool
isMR d = "MR" `isPrefixOf` deviceModel d

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

envNetId :: IO String
envNetId = readFile $ resourceDir <> netFile

baseURL  = "https://api.meraki.com/api/v0"

baseOpts :: IO Network.Wreq.Types.Options
baseOpts = do
  myKey <- myAPIKey
  return $ defaults & header "X-Cisco-Meraki-API-Key" .~ [myKey]
                    & header "Content-Type" .~ ["application/json"]

merakiBaseGet :: String -> IO (Response CL.ByteString)
merakiBaseGet url = do
  opts <- baseOpts
  getWith opts $ baseURL <> url

merakiBasePost :: Postable p => String -> p -> IO (Response CL.ByteString)
merakiBasePost url params = do
  opts <- baseOpts
  postWith opts (baseURL <> url) params


-- response processor: extract response body, parse JSON for
-- a value (declared type a) and grab the Just value
-- unsafe/incomplete/partial because fromJust fails on Nothings
extractAPI :: FromJSON a => Response CL.ByteString -> a
extractAPI resp = fromJust . decode $ (resp ^. responseBody)


---- Writers:
---- write API endpoints to local filesystem locations
--bsnl :: CL.ByteString
--bsnl = CL.singleton '\n'
--
--localWrite :: CL.ByteString -> FilePath -> IO ()
--localWrite s fp = withFile fp WriteMode (flip CL.hPut s)
--
--writeOrg :: MerakiOrg -> IO ()
--writeOrg o = localWrite (encodePretty o) orgFile
--
--writeNet :: MerakiNetwork -> IO ()
--writeNet n = localWrite (encodePretty n) netFile
--
--writeDev :: [MerakiDevice] -> IO ()
--writeDev ds = localWrite (encodePretty ds) devFile
--
--writeSsid :: [MerakiSsid] -> IO ()
--writeSsid ss = localWrite (encodePretty ss) ssidFile
--
-- Getters --
-- local: fetch endpoint objects from local filesystem:
--localMerakiOrg :: IO MerakiOrg
--localMerakiOrg = do
--    s <- CL.readFile orgFile
--    return $ fromJust (decode s :: Maybe MerakiOrg)
--
--localMerakiNet :: IO MerakiNetwork
--localMerakiNet = do
--    s <- CL.readFile netFile
--    return $ fromJust (decode s :: Maybe MerakiNetwork)
--
--localMerakiDev :: IO [MerakiDevice]
--localMerakiDev = do
--    s <- CL.readFile devFile
--    return $ fromJust (decode s :: Maybe [MerakiDevice])
--
--localMerakiSsid :: IO [MerakiSsid]
--localMerakiSsid = do
--    s <- CL.readFile ssidFile
--    return $ fromJust (decode s :: Maybe [MerakiSsid])
--
-- cloud getters: pulls down baseOrg and baseNet from cloud
-- pulls down devices and ssids from baseNet

cloudMerakiOrg :: IO MerakiOrg
cloudMerakiOrg = do
    myOrgId <- envOrgId
    r' <- merakiBaseGet "/organizations"
    return $ fromJust . findOrg myOrgId $ (extractAPI r' :: [MerakiOrg])

cloudMerakiNet :: IO MerakiNetwork
cloudMerakiNet = do
    myNetId <- envNetId
    r' <- cloudMerakiOrg
    r  <- merakiBaseGet $ "/organizations/" <> orgId r' <> "/networks"
    return $ fromJust . findNet myNetId $ (extractAPI r :: [MerakiNetwork])

cloudMerakiDev :: IO [MerakiDevice]
cloudMerakiDev = do
    r' <- cloudMerakiNet
    r  <- merakiBaseGet $ "/networks/" <> networkId r' <> "/devices"
    return $ (extractAPI r :: [MerakiDevice]) 

cloudMerakiSsid :: IO [MerakiSsid]
cloudMerakiSsid = do
    r' <- cloudMerakiNet
    r  <- merakiBaseGet $ "/networks/" <> networkId r' <> "/ssids"
    return $ (extractAPI r :: [MerakiSsid])

-- cache individual endpoints from the cloud to local filesystem locations:
--cacheOrg :: IO ()
--cacheOrg = cloudMerakiOrg >>= writeOrg
--
--cacheNet :: IO ()
--cacheNet = cloudMerakiNet >>= writeNet
--
--cacheDev :: IO ()
--cacheDev = cloudMerakiDev >>= writeDev
--
--cacheSsid :: IO ()
--cacheSsid = cloudMerakiSsid >>= writeSsid
--
--walkingCache :: IO ()
--walkingCache = do
--  myOrg          <- cloudMerakiOrg
--  netResp        <- merakiBaseGet $ "/organizations/" <> orgId myOrg <> "/networks"
--  let myNet      = fromJust $ findNet baseNet $ (extractAPI netResp :: [MerakiNetwork])
--  devResp        <- merakiBaseGet $ "/networks/" <> networkId myNet <> "/devices"
--  let myDevs     = (extractAPI devResp :: [MerakiDevice])
--  ssidResp       <- merakiBaseGet $ "/networks/" <> networkId myNet <> "/ssids"
--  let mySsids    = (extractAPI ssidResp :: [MerakiSsid])
--  CL.putStrLn . encodePretty $ myOrg
--  CL.putStrLn . encodePretty $ myNet
--  CL.putStrLn . encodePretty $ myDevs
--  CL.putStrLn . encodePretty $ mySsids
--  writeOrg myOrg
--  writeNet myNet
--  writeDev myDevs
--  writeSsid mySsids
--

