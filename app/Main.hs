{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Text.IO as TIO
import GHC.IO.Encoding
import Options.Applicative
import Resume
  ( Backend (..),
    compile,
  )
import System.IO

data Args = Args {inputPath :: FilePath, backend :: Backend, outputPath :: FilePath}

parseBackend :: Parser Backend
parseBackend = flag' LaTeX (long "latex") <|> flag' Html (long "html")

parseArgs :: Parser Args
parseArgs =
  Args
    <$> strOption
      (long "file" <> short 'f' <> metavar "FILENAME" <> help "Input file")
    <*> parseBackend
    <*> strOption
      ( long "output" <> short 'o' <> metavar "FILENAME"
          <> help
            "Output file"
      )

main :: IO ()
main = do
  GHC.IO.Encoding.setLocaleEncoding System.IO.utf8
  run =<< execParser opts
  where
    opts =
      info
        (parseArgs <**> helper)
        (fullDesc <> progDesc "Compile resume from Dhall configuration")

run :: Args -> IO ()
run Args {..} =
  compile backend inputPath >>= TIO.writeFile outputPath
