{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module AlexaSkill (generateAlexaSkill) where

import Control.Monad.State.Lazy
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import ZcodeParser

type Text = T.Text

data AlexaSlot = AlexaSlot {
  slotName :: Text,
  slotTypeName :: Text
} deriving (Eq, Show)

data AlexaIntent = AlexaIntent {
  intentName :: Text,
  intentSlots :: [AlexaSlot]
} deriving (Eq, Show)

data AlexaSlotType = AlexaSlotType {
  alexaSlotTypeName :: Text,
  alexaSlotTypeWords :: [Text]
} deriving (Eq, Show)

data AlexaUtterance = AlexaUtterance {
  utteranceIntentName :: Text,
  utteranceWords :: [Text]
} deriving (Eq, Ord, Show)

data AlexaSkill = AlexaSkill {
  alexaIntents :: [AlexaIntent],
  alexaSlotTypes :: [AlexaSlotType],
  alexaUtterances :: [AlexaUtterance]
} deriving (Show)

generateAlexaSkill :: Zcode -> AlexaSkill
generateAlexaSkill zcode =
  AlexaSkill
  (intentsFromZcode zcode)
  (slotTypesFromZcode zcode)
  (setToList $ utterancesFromZcode zcode)

intentsFromZcode :: Zcode -> [AlexaIntent]
intentsFromZcode zcode = []

slotTypesFromZcode :: Zcode -> [AlexaSlotType]
slotTypesFromZcode Zcode{..} = Map.foldrWithKey toSlots [] zcodeWords
  where
    toSlots key value slots = (AlexaSlotType key value):slots

utterancesFromZcode :: Zcode -> Set.Set AlexaUtterance
utterancesFromZcode Zcode{..} =
  foldr Set.insert Set.empty (map utterance zcodeUtterances)
  where
    utterance :: [ZWord] -> AlexaUtterance
    utterance zwords =
      let (words, counts) = runState (sequence $ map toWord zwords) Map.empty
      in AlexaUtterance (intentName counts) words

    countSlots :: Text -> State (Map.Map Text Int) Int
    countSlots name = do
      slotCounts <- get
      let count = (Map.findWithDefault 0 name slotCounts) + 1
      put $ Map.insert name count slotCounts
      return count

    slotName :: String -> Int -> State (Map.Map Text Int) Text
    slotName root index =
      return . T.pack $ "{" ++ root ++ show index ++ "}"

    toWord :: ZWord -> State (Map.Map Text Int) Text
    toWord (Verb v) = countSlots "verb" >>= slotName "Verb"
    toWord Noun = countSlots "noun" >>= slotName "Noun"
    toWord Direction = countSlots "dir" >>= slotName "Direction"
    toWord (Fixed f) = return f

    intentName :: Map.Map Text Int -> Text
    intentName slotCounts =
      let
        part key str = case Map.lookup key slotCounts of
          Nothing -> ""
          (Just n) -> T.intercalate "" (replicate n str)
        verbPart = part "verb" "Verb"
        nounPart = part "noun" "Noun"
        dirPart = part "dir" "Direction"
      in
        T.intercalate "" [verbPart, nounPart, dirPart, "Intent"]

setToList :: Set.Set a -> [a]
setToList = foldr (:) []
