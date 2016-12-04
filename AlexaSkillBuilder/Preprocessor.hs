{-# LANGUAGE OverloadedStrings #-}
module Preprocessor (parsePreprocessorFile) where

import Data.Map
import qualified Data.Text as T

type Text = T.Text
type Deletions = [Text]
type Substitutions = Map Text Text

parsePreprocessorFile :: FilePath -> IO (Text -> Maybe Text)
parsePreprocessorFile path = fmap preprocess (readPreprocessorFile path)

readPreprocessorFile :: FilePath -> IO (Deletions, Substitutions)
readPreprocessorFile path = do
  content <- readFile path
  let directives = T.lines . T.pack $ content
  return $ addDirectives [] empty directives

addDirectives :: Deletions -> Substitutions -> [Text] -> (Deletions, Substitutions)
addDirectives dels subs [] = (dels, subs)
addDirectives dels subs (d:ds) =
  if T.null d then
    addDirectives dels subs ds
  else if isDeletion d then
    addDirectives (T.tail d:dels) subs ds
  else if isSubtitution d then
    addDirectives dels (addSubtitution d $ subs) ds
  else
    addDirectives dels subs ds
  where
    isDeletion t = T.head t == '-'
    isSubtitution t = "=" `T.isInfixOf` t
    addSubtitution t = let p = T.splitOn "=" t in insert (head p) (last p)

preprocess :: (Deletions, Substitutions) -> Text -> Maybe Text
preprocess (dels, subs) t =
  if t `elem` dels then Nothing
  else Just $ findWithDefault t t subs
