module CV where

import Data.Text
import Dhall

import CV.Types

parseCV :: Text -> IO (CV Text)
parseCV = input auto
