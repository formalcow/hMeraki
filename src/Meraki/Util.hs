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

baseOpts :: IO Network.Wreq.Types.Options
baseOpts = do
  myKey <- myAPIKey
  return $ defaults & header "X-Cisco-Meraki-API-Key" .~ [myKey]
                    & header "Content-Type" .~ ["application/json"]

merakiApiGet :: String -> IO (Response CL.ByteString)
merakiApiGet url = do
  opts <- baseOpts
  getWith opts $ baseURL <> url

merakiApiPost :: Postable p => String -> p -> IO (Response CL.ByteString)
merakiApiPost url params = do
  opts <- baseOpts
  postWith opts (baseURL <> url) params

-- response processor: extract response body, parse JSON for
-- a value (declared type a) and grab the Just value
-- unsafe/incomplete/partial because fromJust fails on Nothings
processAPI :: FromJSON a => Response CL.ByteString -> a
processAPI resp = fromJust . decode $ (resp ^. responseBody)

cloudMerakiOrg :: IO MerakiOrg
cloudMerakiOrg = do
  myOrgId <- envOrgId
  r' <- merakiApiGet $ "/organization/" <> myOrgId
  return $ (processAPI r' :: MerakiOrg)

cloudMerakiNet :: IO MerakiNetwork
cloudMerakiNet = do
  myNetId <- envNetId
  r' <- cloudMerakiOrg
  r  <- merakiApiGet $ "/organizations/" <> orgId r' <> "/networks"
  return $ fromJust . findNet myNetId $ (processAPI r :: [MerakiNetwork])

cloudMerakiDev :: IO [MerakiDevice]
cloudMerakiDev = do
  r' <- cloudMerakiNet
  r  <- merakiApiGet $ "/networks/" <> networkId r' <> "/devices"
  return $ (processAPI r :: [MerakiDevice]) 

cloudMerakiSsid :: IO [MerakiSsid]
cloudMerakiSsid = do
  r' <- cloudMerakiNet
  r  <- merakiApiGet $ "/networks/" <> networkId r' <> "/ssids"
  return $ (processAPI r :: [MerakiSsid])
