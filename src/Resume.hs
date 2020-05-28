{-# LANGUAGE OverloadedStrings #-}

module Resume
  ( Backend (..),
    HtmlOptions (..),
    LaTeXOptions (..),
    compile,
    def,
    defaultInputSettings,
    parseResume,
    readResume,
    rootDirectory,
  )
where

import Data.Text.IO as TIO
import Dhall
import Resume.Backend.Html
import Resume.Backend.LaTeX
import Resume.Types

data Backend
  = LaTeX LaTeXOptions
  | Html HtmlOptions

parseResume :: InputSettings -> Text -> IO (Resume Markdown)
parseResume settings inp =
  fmap Markdown
    <$> inputWithSettings settings auto inp

readResume :: InputSettings -> FilePath -> IO (Resume Markdown)
readResume settings path = TIO.readFile path >>= parseResume settings

-- | Compile resume using selected backend.
-- Interprets text fields as Markdown.
compile :: Backend -> FilePath -> IO Text
compile backend path = do
  r <- readResume defaultInputSettings path
  let compiler = case backend of
        LaTeX opts -> Resume.Backend.LaTeX.renderText opts
        Html opts -> Resume.Backend.Html.renderText opts
  case compiler r of
    Right t -> return t
    Left e -> error $ "Pandoc error: " <> show e
