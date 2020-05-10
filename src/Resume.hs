{-# LANGUAGE OverloadedStrings #-}

module Resume
  ( Backend (..),
    compile,
    readMarkdownResume,
  )
where

import Dhall
import Resume.Backend.Html
  ( compileHtml,
    def,
  )
import Resume.Backend.LaTeX (compileLaTeX)
import Resume.Types

data Backend
  = LaTeX
  | Html
  deriving (Eq, Show)

readMarkdownResume :: Text -> IO (Resume Markdown)
readMarkdownResume = (fmap . fmap) Markdown . input auto

-- | Compile resume using selected backend. Interprets text fields as Markdown.
compile :: Backend -> Text -> IO Text
compile backend inp = do
  r <- readMarkdownResume inp
  let compiler = case backend of
        LaTeX -> compileLaTeX
        Html -> compileHtml def
  case compiler r of
    Right t -> return t
    Left e -> error $ "Pandoc error: " <> show e
