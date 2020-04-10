{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO                  as TIO
import           Options.Applicative

import           CV                             ( compile )

data Args = Args { inputPath :: FilePath, outputPath :: FilePath }

args :: Parser Args
args =
  Args
    <$> strOption
          (long "file" <> short 'f' <> metavar "FILENAME" <> help "Input file")
    <*> strOption
          (long "output" <> short 'o' <> metavar "FILENAME" <> help
            "Output file"
          )

main :: IO ()
main = run =<< execParser opts
 where
  opts = info (args <**> helper)
              (fullDesc <> progDesc "Compile CV from Dhall configuration")

run :: Args -> IO ()
run Args {..} = TIO.readFile inputPath >>= compile >>= TIO.writeFile outputPath
