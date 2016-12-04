{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import Options.Applicative
import Data.Monoid ((<>))
import AlexaSkill
import Preprocessor
import ZcodeParser

data Arguments = Arguments {
  argZcodePath :: FilePath,
  argInfodumpPath :: FilePath,
  argPreprocessorPath :: FilePath
}

arguments :: Parser Arguments
arguments = Arguments
  <$> strOption (long "zfile" <> short 'z' <> help "path to zcode file")
  <*> strOption (long "infodump" <> short 'i' <> help "path to infodump executable")
  <*> strOption (long "preprocessor" <> short 'p' <> help "path to preprocessor directives file")

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
  putStrLn $ "zcode " ++ (show zcode)
  let skill = generateAlexaSkill zcode
  putStrLn "done"
