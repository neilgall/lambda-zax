{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module AlexaSkill (
  generateAlexaSkill,
  intentNameForUtterance,
  AlexaSlot(..),
  AlexaSlotWords(..),
  AlexaSlotType(..),
  AlexaIntent(..),
  AlexaUtterance(..),
  AlexaSkill(..)
) where

import Control.Monad.State.Lazy
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import ZcodeParser

type Text = T.Text

data AlexaSlotType = VerbSlot | NounSlot | DirectionSlot
  deriving (Eq, Ord)

data AlexaSlot = AlexaSlot {
  slotName :: Text,
  slotType :: AlexaSlotType
} deriving (Eq, Ord, Show)

data AlexaIntent = AlexaIntent {
  intentName :: Text,
  intentSlots :: [AlexaSlot]
} deriving (Eq, Ord, Show)

data AlexaSlotWords = AlexaSlotWords {
  alexaSlotWordsName :: Text,
  alexaSlotWordsOptions :: [Text]
} deriving (Eq, Show)

data AlexaUtterance = AlexaUtterance {
  utteranceWords :: [Text],
  utteranceSlotCounts :: Map.Map AlexaSlotType Int
} deriving (Eq, Ord, Show)

data AlexaSkill = AlexaSkill {
  alexaIntents :: [AlexaIntent],
  alexaSlotWords :: [AlexaSlotWords],
  alexaUtterances :: [AlexaUtterance]
} deriving (Show)

generateAlexaSkill :: Zcode -> AlexaSkill
generateAlexaSkill zcode =
  let
    utterancesSet = utterancesFromZcode zcode
    intentsSet = intentsFromUtterances utterancesSet
  in
    AlexaSkill
      (setToList intentsSet)
      (slotWordsFromZcode zcode)
      (setToList utterancesSet)

intentsFromUtterances :: Set.Set AlexaUtterance -> Set.Set AlexaIntent
intentsFromUtterances utterances = Set.map toIntent utterances
  where
    toIntent utterance = AlexaIntent
      (intentNameForUtterance utterance)
      (intentSlotsForUtterance utterance)

slotWordsFromZcode :: Zcode -> [AlexaSlotWords]
slotWordsFromZcode Zcode{..} = Map.foldrWithKey toSlots [] zcodeWords
  where
    toSlots key value slots = (AlexaSlotWords key value):slots

utterancesFromZcode :: Zcode -> Set.Set AlexaUtterance
utterancesFromZcode Zcode{..} =
  foldr Set.insert Set.empty (map utterance zcodeUtterances)
  where
    utterance :: [ZWord] -> AlexaUtterance
    utterance zwords =
      let (words, counts) = runState (sequence $ map toWord zwords) Map.empty
      in AlexaUtterance words counts

    countSlots :: AlexaSlotType -> State (Map.Map AlexaSlotType Int) Int
    countSlots slotType = do
      slotCounts <- get
      let count = (Map.findWithDefault 0 slotType slotCounts) + 1
      put $ Map.insert slotType count slotCounts
      return count

    toWord :: ZWord -> State (Map.Map AlexaSlotType Int) Text
    toWord (Verb v) = fmap (slotNameForTypeAndIndex VerbSlot) (countSlots VerbSlot)
    toWord Noun = fmap (slotNameForTypeAndIndex NounSlot) (countSlots NounSlot)
    toWord Direction = fmap (slotNameForTypeAndIndex DirectionSlot) (countSlots DirectionSlot)
    toWord (Fixed f) = return f

instance Show AlexaSlotType where
  show VerbSlot = "Verb"
  show NounSlot = "Noun"
  show DirectionSlot = "Direction"

slotNameForTypeAndIndex :: AlexaSlotType -> Int -> Text
slotNameForTypeAndIndex slotType index =
  T.concat ["{", showT slotType, showT index, "}"]

intentNameForUtterance :: AlexaUtterance -> Text
intentNameForUtterance AlexaUtterance{..} =
  let
    count slotType = case Map.lookup slotType utteranceSlotCounts of
      Nothing -> ""
      (Just n) -> T.concat $ replicate n (showT slotType)
  in
    T.concat [count VerbSlot, count NounSlot, count DirectionSlot, "Intent"]

intentSlotsForUtterance :: AlexaUtterance -> [AlexaSlot]
intentSlotsForUtterance AlexaUtterance{..} = Map.foldrWithKey toSlots [] utteranceSlotCounts
  where
    toSlots :: AlexaSlotType -> Int -> [AlexaSlot] -> [AlexaSlot]
    toSlots _ 0 slots = slots
    toSlots slotType n slots =
      (mkSlot slotType n):(toSlots slotType (n-1) slots)
    mkSlot t n = AlexaSlot (slotNameForTypeAndIndex t n) t

setToList :: Set.Set a -> [a]
setToList = foldr (:) []

showT :: Show a => a -> Text
showT = T.pack . show
