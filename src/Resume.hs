{-# LANGUAGE OverloadedStrings #-}

module Resume
  ( Backend (..),
    compile,
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
  = LaTeX
  | Html
  deriving (Eq, Show)

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
        LaTeX -> Resume.Backend.LaTeX.renderText def
        Html -> Resume.Backend.Html.renderText def
  case compiler r of
    Right t -> return t
    Left e -> error $ "Pandoc error: " <> show e
