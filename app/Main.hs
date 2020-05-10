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

data Args = Args {inputPath :: FilePath, _backend :: Backend, outputPath :: FilePath}

backend :: Parser Backend
backend = flag' LaTeX (long "latex") <|> flag' Html (long "html")

args :: Parser Args
args =
  Args
    <$> strOption
      (long "file" <> short 'f' <> metavar "FILENAME" <> help "Input file")
    <*> backend
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
        (args <**> helper)
        (fullDesc <> progDesc "Compile resume from Dhall configuration")

run :: Args -> IO ()
run Args {..} =
  TIO.readFile inputPath >>= compile _backend >>= TIO.writeFile outputPath
