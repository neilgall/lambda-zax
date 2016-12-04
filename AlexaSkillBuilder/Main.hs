{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import Data.Monoid ((<>))
import Options.Applicative

import AlexaSkill
import AlexaSkillOutput
import Preprocessor
import ZcodeParser

data Arguments = Arguments {
  argZcodePath :: FilePath,
  argInfodumpPath :: FilePath,
  argPreprocessorPath :: FilePath,
  argOutputPath :: FilePath
}

arguments :: Parser Arguments
arguments = Arguments
  <$> strOption (long "zfile" <> short 'z' <> help "path to zcode file")
  <*> strOption (long "infodump" <> short 'i' <> help "path to infodump executable")
  <*> strOption (long "preprocessor" <> short 'p' <> help "path to preprocessor directives file")
  <*> strOption (long "output" <> short 'o' <> help "path to output directory")

argumentParser :: IO Arguments
argumentParser = execParser $ info (helper <*> arguments) description
  where
    description = fullDesc
      <> progDesc "Generate an Alexa Skill"
      <> header "alexaSkillBuilder - Generate Alexa Skill definitions from zcode files"

main :: IO ()
main = do
  Arguments{..} <- argumentParser
  preprocessor <- parsePreprocessorFile argPreprocessorPath
  zcode <- parseZcodeFile argZcodePath argInfodumpPath preprocessor
  let skill = generateAlexaSkill zcode
  outputAlexaSkill argOutputPath skill
