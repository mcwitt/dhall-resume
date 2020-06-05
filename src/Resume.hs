{-# LANGUAGE OverloadedStrings #-}

module Resume
  ( Backend (..),
    HtmlOptions (..),
    LaTeXOptions (..),
    compile,
    def,
    defaultInputSettings,
    parseResume,
    toPandoc,
    rootDirectory,
  )
where

import Data.Text.IO as TIO
import Dhall
import Resume.Backend.Html
import Resume.Backend.LaTeX
import Resume.Types
import Text.Pandoc (Pandoc, PandocError)
import qualified Text.Pandoc as P

data Backend
  = LaTeX LaTeXOptions
  | Html HtmlOptions

-- | Deserialize dhall input
parseResume :: InputSettings -> Text -> IO (Resume Text)
parseResume settings = inputWithSettings settings auto

-- | Parse resume text fields as Pandoc Markdown
toPandoc :: Resume Text -> Either PandocError (Resume Pandoc)
toPandoc = P.runPure . traverse (P.readMarkdown def)

readResume :: InputSettings -> FilePath -> IO (Resume Pandoc)
readResume settings path = do
  raw <- TIO.readFile path
  text <- parseResume settings raw
  case toPandoc text of
    Right res -> return res
    Left err -> fail $ "Error parsing Pandoc Markdown: " ++ show err

-- | Generate resume from dhall file using the specified backend
compile :: Backend -> FilePath -> IO Text
compile backend path = do
  r <- readResume defaultInputSettings path
  let compiler = case backend of
        LaTeX opts -> Resume.Backend.LaTeX.renderText opts
        Html opts -> Resume.Backend.Html.renderText opts
  case compiler r of
    Right t -> return t
    Left e -> error $ "Rendering error: " <> show e
