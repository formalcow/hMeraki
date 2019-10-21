{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Meraki.Types where

import GHC.Generics
import Data.Scientific
import Data.Aeson
import Text.Casing
import Prelude

type Tag = String
type Serial = String

data MerakiModel
  = MR
  | MS
  | MX
  | MV
  deriving (Show, Eq)

data MerakiOrg = MerakiOrg {
    orgId                  :: String
  , orgName                :: String
  } deriving (Generic, Show, Eq)

merakiOrgLabel :: String -> String
merakiOrgLabel = camel . drop 3

instance ToJSON MerakiOrg where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = merakiOrgLabel }

instance FromJSON MerakiOrg where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier =  merakiOrgLabel }

data MerakiAdmin = MerakiAdmin {
    adminName                  :: String
  , adminEmail                 :: String
  , adminId                    :: String
  , adminNetworks              :: [MerakiNetwork]
  , adminTags                  :: [String]
  , adminTwoFactorAuthEnabled  :: Bool
  , adminLastActive            :: Scientific
  , adminAccountStatus         :: String
  , adminHasApiKey             :: Bool
  , adminOrgAccess             :: String
  } deriving (Generic, Show, Eq)

merakiAdminLabel :: String -> String
merakiAdminLabel =  camel . drop 5

instance ToJSON MerakiAdmin where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = merakiAdminLabel }

instance FromJSON MerakiAdmin where
  parseJSON = genericParseJSON defaultOptions {
             fieldLabelModifier = merakiAdminLabel }

data MerakiNetwork = MerakiNetwork {
    networkId                      :: String
  , networkOrganizationId          :: String
  , networkName                    :: String
  , networkTimeZone                :: String
  , networkTags                    :: Maybe String
  , networkType                    :: String
  , networkConfigTemplateId        :: Maybe String
  , networkDisableMyMerakiCom      :: Maybe Bool
  , networkDisableRemoteStatusPage :: Bool
  } deriving (Generic, Show, Eq)

merakiNetworkLabel :: String -> String
merakiNetworkLabel = camel . drop 7

instance ToJSON MerakiNetwork where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = merakiNetworkLabel }

instance FromJSON MerakiNetwork where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier =  merakiNetworkLabel }

data MerakiSsid = MerakiSsid {
     ssidName                            :: Maybe String
   , ssidNumber                          :: Scientific
   , ssidEnabled                         :: Bool
   , ssidMinBitrate                      :: Maybe Scientific
   , ssidAuthMode                        :: String
   , ssidPsk                             :: Maybe String
   , ssidEncryptionMode                  :: Maybe String
   , ssidWpaEncryptionMode               :: Maybe String
   , ssidSplashPage                      :: String
   , ssidWalledGardenEnabled             :: Maybe Bool
   , ssidWalledGardenRange               :: Maybe String
   , ssidIpAssignmentMode                :: String
   , ssidUseVlanTagging                  :: Maybe Bool
   , ssidPerClientBandwidthLimitDown     :: Scientific
   , ssidPerClientBandwidthLimitUp       :: Scientific
   , ssidBandSelection                   :: String
   , ssidRadiusServers                   :: Maybe [MerakiRADIUSServer]
   , ssidRadiusCoaEnabled                :: Maybe Bool
   , ssidRadiusFailoverPolicy            :: Maybe String
   , ssidRadiusLoadBalancingPolicy       :: Maybe String
   , ssidRadiusAccountingPolicy          :: Maybe String
   , ssidRadiusAccountingServers         :: Maybe [MerakiRADIUSServer]
   , ssidRadiusAttributeForGroupPolicies :: Maybe String
   , ssidRadiusOverride                  :: Maybe Bool
   , ssidConcentratorNetworkId           :: Maybe String
   , ssidVlanId                          :: Maybe Scientific
   , ssidDefaultVlanId                   :: Maybe Scientific
   , ssidApTagsaAndVlans                 :: Maybe [MerakiApTagsAndVlans]
   , ssidLanIsolationEnabled             :: Maybe Bool
   , ssidAdminAccessible                 :: Maybe Bool
   } deriving (Generic, Show, Eq)

merakiSsidLabel :: String -> String
merakiSsidLabel = camel . drop 4

instance ToJSON MerakiSsid where
  toJSON = genericToJSON defaultOptions {
            fieldLabelModifier = merakiSsidLabel }

instance FromJSON MerakiSsid where
  parseJSON = genericParseJSON defaultOptions {
               fieldLabelModifier = merakiSsidLabel }

data MerakiRADIUSServer = MerakiRADIUSServer {
    radiusServerHost                         :: String
  , radiusServerPort                         :: Scientific
  , radiusServerSecret                       :: String
  } deriving (Generic, Show, Eq)

merakiRadiusServerLabel :: String -> String
merakiRadiusServerLabel = camel . drop 6

instance ToJSON MerakiRADIUSServer where
  toJSON = genericToJSON defaultOptions {
            fieldLabelModifier = merakiRadiusServerLabel }

instance FromJSON MerakiRADIUSServer where
  parseJSON = genericParseJSON defaultOptions {
              fieldLabelModifier = merakiRadiusServerLabel }

data MerakiApTagsAndVlans = MerakiApTagsAndVlans {
    mApTags                                      :: String
  , mApVlanIds                                   :: Scientific
  } deriving (Generic, Show, Eq)

merakiApTagsAndVlansLabel :: String -> String
merakiApTagsAndVlansLabel = camel . drop 6

instance ToJSON MerakiApTagsAndVlans where
  toJSON = genericToJSON defaultOptions {
            fieldLabelModifier = merakiApTagsAndVlansLabel }

instance FromJSON MerakiApTagsAndVlans where
  parseJSON = genericParseJSON defaultOptions {
            fieldLabelModifier = merakiApTagsAndVlansLabel }

data MerakiDevice = MerakiDevice {
     deviceName                  :: Maybe String
   , deviceLat                   :: Scientific
   , deviceLng                   :: Scientific
   , deviceSerial                :: Serial
   , deviceMac                   :: String
   , deviceModel                 :: String
   , deviceAddress               :: String
   , deviceNotes                 :: Maybe String
   , deviceLanIp                 :: Maybe String
   , deviceTags                  :: Maybe String
   , deviceNetworkId             :: String
   , deviceBeaconIdParams        :: Maybe BeaconIdParams
   } deriving (Generic, Show, Eq)

merakiDeviceLabel :: String -> String
merakiDeviceLabel = camel . drop 6

instance ToJSON MerakiDevice where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = merakiDeviceLabel }

instance FromJSON MerakiDevice where
  parseJSON = genericParseJSON defaultOptions {
               fieldLabelModifier = merakiDeviceLabel }

data BeaconIdParams = BeaconIdParams {
     beaconUuid                      :: String
   , beaconMajor                     :: Scientific
   , beaconMinor                     :: Scientific
   } deriving (Generic, Show, Eq)

beaconIdParamsLabel :: String -> String
beaconIdParamsLabel = camel . drop 6

instance ToJSON BeaconIdParams where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = beaconIdParamsLabel }

instance FromJSON BeaconIdParams where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = beaconIdParamsLabel }

data MerakiCameraZone = MerakiCameraZone {
     mvZoneZoneId                        :: String
   , mvZoneType                          :: String
   , mvZoneLabel                         :: String
   } deriving (Generic, Show, Eq)

merakiCameraZoneLabel :: String -> String
merakiCameraZoneLabel = camel . drop 6

instance ToJSON MerakiCameraZone where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = merakiCameraZoneLabel }

instance FromJSON MerakiCameraZone where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = merakiCameraZoneLabel }
