module CV
  ( compile
  , readMarkdownCV
  )
where

import           Dhall
import           CV.Backend.LaTeX               ( compileLaTeX )
import           CV.Types

readMarkdownCV :: Text -> IO (CV Markdown)
readMarkdownCV = (fmap . fmap) Markdown . input auto

-- | Interpret input as Dhall with Markdown-formatted text fields and output LaTeX
compile :: Text -> IO Text
compile inp = do
  cv <- readMarkdownCV inp
  case compileLaTeX cv of
    Right t -> return t
    Left  e -> error $ "Pandoc error: " <> show e
