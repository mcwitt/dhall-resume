module Resume
  ( compile
  , readMarkdownResume
  )
where

import           Dhall
import           Resume.Backend.LaTeX           ( compileLaTeX )
import           Resume.Types

readMarkdownResume :: Text -> IO (Resume Markdown)
readMarkdownResume = (fmap . fmap) Markdown . input auto

-- | Interpret input as Dhall with Markdown-formatted text fields and output LaTeX
compile :: Text -> IO Text
compile inp = do
  r <- readMarkdownResume inp
  case compileLaTeX r of
    Right t -> return t
    Left  e -> error $ "Pandoc error: " <> show e
