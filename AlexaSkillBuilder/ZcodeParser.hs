{-# LANGUAGE OverloadedStrings #-}
module ZcodeParser (Zcode(..), ZWord(..), parseZcodeFile) where

import Debug.Trace
import Data.Maybe (catMaybes, maybeToList)
import qualified Data.Map as M
import qualified Data.Text as T
import System.Process
import Text.Regex

type Text = T.Text
data ZWord = Verb Text
           | Fixed Text
           | Noun
           | Direction
           deriving (Show)

type Preprocessor = Text -> [Text]
type Utterance = [ZWord]
type ZWordsByType = M.Map Text [Text]

data Zcode = Zcode {
  zcodeWords :: ZWordsByType,
  zcodeUtterances :: [Utterance]
} deriving (Show)

invokeInfoDump :: FilePath -> FilePath -> [String] -> IO [Text]
invokeInfoDump infoDump zcode options =
  fmap (T.lines . T.pack) $ readProcess infoDump (options ++ [zcode]) ""

parseZcodeFile :: FilePath -> FilePath -> (Text -> Maybe Text) -> IO Zcode
parseZcodeFile zcodePath infodumpPath preprocessor =
  let
    preprocessor' = maybeToList . preprocessor
    infodump = invokeInfoDump infodumpPath zcodePath
  in do
    zobjects <- infodump ["-w0", "-c1", "-d"]
    grammar <- infodump ["-w0", "-s", "-g"]
    return $ Zcode (wordsByType preprocessor' zobjects) (utterances preprocessor' grammar)

matchRegexT :: Regex -> Text -> [Text]
matchRegexT r = map T.pack . concat . matchRegex r . T.unpack

wordsByType :: Preprocessor -> [Text] -> ZWordsByType
wordsByType preprocessor = wordsByTypes' M.empty
  where
    wordRegex = mkRegex "\\$[0-9a-f]+ +([a-z]+).*<([a-z]+)>"
    wordsByTypes' words [] = words
    wordsByTypes' words (line:lines) =
      case matchRegexT wordRegex line >>= preprocessor of
        (word:key:[]) ->
          let words' = M.insertWith mappend key [word] words
          in wordsByTypes' words' lines
        _ ->
          wordsByTypes' words lines

utterances :: Preprocessor -> [Text] -> [Utterance]
utterances preprocessor = findVerb []
  where
    findVerb :: [Utterance] -> [Text] -> [Utterance]
    findVerb utts [] = utts
    findVerb utts (line:lines) =
      if "verb = " `T.isInfixOf` line then
        let
          wordRegex = mkRegex "\"([a-z]+)\""
          verbs = T.words line >>= matchRegexT wordRegex >>= preprocessor
        in
          findUtts utts verbs lines
      else
        findVerb utts lines

    findUtts :: [Utterance] -> [Text] -> [Text] -> [Utterance]
    findUtts utts _ [] = utts
    findUtts utts verbs (line:lines) =
      let
        sentenceRegex = mkRegex "\"([a-zA-Z0-9 ]+)\""
        sentenceWords = matchRegexT sentenceRegex line >>= T.words >>= preprocessor
        sentence = parseSentence sentenceWords verbs
      in
        if null sentenceWords then
          findVerb utts lines
        else
          findUtts (sentence:utts) verbs lines

parseSentence :: [Text] -> [Text] -> Utterance
parseSentence words verbs = reverse $ parseSentence' [] words
  where
    objType = if "go" `elem` verbs then Direction else Noun
    parseSentence' zwords [] = zwords
    parseSentence' zwords (w:ws) =
      if w `elem` verbs then
        parseSentence' (Verb w:zwords) ws
      else if w == "OBJ" then
        parseSentence' (objType:zwords) ws
      else
        parseSentence' (Fixed w:zwords) ws
