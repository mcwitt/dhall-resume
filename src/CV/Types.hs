{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Type definitions mirroring the Dhall definitions. The latter should be
considered source-of-truth; the duplication here is intended to be
temporary and should be removed with the eventual transition to
generating Haskell type definitions from the Dhall definitions
(e.g. using 'Dhall.TH').
-}

module CV.Types where

import           Data.Text
import           Dhall

data Name = Name { first :: Text, last :: Text } deriving (Eq, Generic, Show)

data ContactInfo = ContactInfo { email    :: Text
                               , linkedin :: Text
                               , twitter  :: Maybe Text
                               , phone    :: Text
                               } deriving (Eq, Generic, Show)

data CVDate = CVDate { year  :: Natural
                     , month :: Natural
                     } deriving (Eq, Generic, Show)

data SkillCategory = SkillCategory { category :: Text
                                   , keywords :: [Text]
                                   } deriving (Eq, Generic, Show)

-- The type parameter 'a' in the types below refers to underlying text
-- representation. The input type is always 'CV Text'.  This might be
-- converted to a different representation before rendering depending
-- on the desired output format.

data CV a = CV { name     :: Name
               , headline :: Maybe Text
               , city     :: Text
               , contact  :: ContactInfo
               , sections :: [CVSection a]
               } deriving (Eq, Functor, Generic, Show)

data CVSection a = CVSection { heading :: Text
                             , content :: SectionContent a
                             } deriving (Eq, Functor, Generic, Show)

data SectionContent a = Paragraph a
                      | Experiences [Experience a]
                      | Skills [SkillCategory]
                      deriving (Eq, Functor, Generic, Show)

data Experience a = Experience { title        :: Text
                               , organization :: Text
                               , location     :: Text
                               , startMonth   :: CVDate
                               , endMonth     :: Maybe CVDate
                               , items        :: [a]
                               } deriving (Eq, Functor, Generic, Show)

instance FromDhall CVDate
instance FromDhall ContactInfo
instance FromDhall Name
instance FromDhall SkillCategory

instance FromDhall a => FromDhall (Experience a)
instance FromDhall a => FromDhall (SectionContent a)
instance FromDhall a => FromDhall (CVSection a)
instance FromDhall a => FromDhall (CV a)
