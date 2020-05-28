{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Text.IO as TIO
import GHC.IO.Encoding
import Options.Applicative
import Resume
import System.IO

data Args = Args {_inputPath :: FilePath, _backend :: Backend, _outputPath :: FilePath}

args :: Parser Args
args =
  Args
    <$> strOption
      ( long "file"
          <> short 'f'
          <> metavar "FILENAME"
          <> help "Input file"
      )
    <*> backend
    <*> strOption
      ( long "output"
          <> short 'o'
          <> metavar "FILENAME"
          <> help "Output file"
      )

backend :: Parser Backend
backend = latex <|> html

latex :: Parser Backend
latex = flag' LaTeX (long "latex") <*> latexOptions

latexOptions :: Parser LaTeXOptions
latexOptions =
  LaTeXOptions
    <$> optional
      ( strOption
          ( long "bibfile"
              <> metavar "FILENAME"
              <> help "BibTeX file containing publications"
          )
      )

html :: Parser Backend
html = flag' Html (long "html") <*> pure def

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
run Args {..} = compile _backend _inputPath >>= TIO.writeFile _outputPath
