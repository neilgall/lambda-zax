{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module AlexaSkillOutput (outputAlexaSkill) where

import Control.Monad (forM_)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (combine)

import AlexaSkill

outputAlexaSkill :: FilePath -> AlexaSkill -> IO ()
outputAlexaSkill dir AlexaSkill{..} = do
  createDirectoryIfMissing True dir
  outputIntentSchema (combine dir "intent.schema") alexaIntents
  outputUtterances (combine dir "utterances.txt") alexaUtterances
  forM_ alexaSlotWords (outputSlotWords dir)

outputIntentSchema :: FilePath -> [AlexaIntent] -> IO ()
outputIntentSchema filepath intents =
  BS.writeFile filepath $ encodePretty (IntentSchema intents)

outputUtterances :: FilePath -> [AlexaUtterance] -> IO ()
outputUtterances filepath utterances =
  let
    toText u = T.intercalate " " (intentNameForUtterance u:utteranceWords u)
    utteranceLines = map toText utterances
    body = T.unpack . T.unlines $ utteranceLines
  in
    writeFile filepath body

outputSlotWords :: FilePath -> AlexaSlotWords -> IO ()
outputSlotWords dir AlexaSlotWords{..} =
  let
    filename = (T.unpack alexaSlotWordsName) ++ ".txt"
    filepath = combine dir filename
    body = T.unpack $ T.unlines alexaSlotWordsOptions
  in do
    writeFile filepath body

data IntentSchema = IntentSchema [AlexaIntent]

instance ToJSON IntentSchema where
  toJSON (IntentSchema intents) =
    object [ "intents" .= intents ]

instance ToJSON AlexaIntent where
  toJSON AlexaIntent{..} =
    object [ "intent" .= intentName,
             "slots" .= intentSlots ]

instance ToJSON AlexaSlot where
  toJSON AlexaSlot{..} =
    object [ "name" .= slotName,
             "type" .= show slotType ]
