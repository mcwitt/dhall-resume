{-# LANGUAGE OverloadedStrings #-}

module Resume
  ( Backend (..),
    compile,
    parseResume,
    readResume,
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

parseResume :: Text -> IO (Resume Markdown)
parseResume = (fmap . fmap) Markdown . input auto

readResume :: FilePath -> IO (Resume Markdown)
readResume path = TIO.readFile path >>= parseResume

-- | Compile resume using selected backend. Interprets text fields as Markdown.
compile :: Backend -> FilePath -> IO Text
compile backend path = do
  r <- readResume path
  let compiler = case backend of
        LaTeX -> Resume.Backend.LaTeX.renderText
        Html -> Resume.Backend.Html.renderText def
  case compiler r of
    Right t -> return t
    Left e -> error $ "Pandoc error: " <> show e
