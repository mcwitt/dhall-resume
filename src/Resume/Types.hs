{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

-- |
-- Type definitions mirroring the Dhall definitions. The latter should be
-- considered source-of-truth; the duplication here is intended to be
-- temporary and should be removed with the eventual transition to
-- generating Haskell type definitions from the Dhall definitions
-- (e.g. using 'Dhall.TH').
module Resume.Types where

import Data.Text
import Dhall

type CitationKey = Text

data Basics
  = Basics
      { name :: Maybe Name,
        email :: Text,
        location :: Location,
        phone :: Maybe Text
      }
  deriving (Generic, FromDhall, Eq, Show)

data Name
  = Name
      { firstName :: Text,
        lastName :: Text
      }
  deriving (Generic, FromDhall, Eq, Show)

data Location
  = StreetAddress
      { address :: Text,
        city :: Text,
        postalCode :: Text,
        country :: Maybe Text
      }
  | Region Text
  deriving (Generic, FromDhall, Eq, Show)

data Profiles
  = Profiles
      { homepage :: Maybe Link,
        linkedin :: Maybe Social,
        github :: Maybe Social,
        twitter :: Maybe Social
      }
  deriving (Generic, FromDhall, Eq, Show)

data Date
  = Date
      { year :: Natural,
        month :: Natural
      }
  deriving (Generic, FromDhall, Eq, Show)

data Language
  = Language
      { language :: Text,
        fluency :: Text
      }
  deriving (Generic, FromDhall, Eq, Show)

data Link
  = Link
      { url :: Text,
        label :: Maybe Text
      }
  deriving (Generic, FromDhall, Eq, Show)

data Social
  = Social
      { user :: Text,
        profileUrl :: Maybe Text
      }
  deriving (Generic, FromDhall, Eq, Show)

-- The type parameter 'a' in the types below refers to underlying text
-- representation. The input type is always 'Resume Text'.  This might be
-- converted to a different representation before rendering depending
-- on the desired output format.

data Resume a
  = Resume
      { basics :: Basics,
        profiles :: Profiles,
        headline :: Maybe a,
        sections :: [Section a]
      }
  deriving (Functor, Foldable, Traversable, Generic, Eq, Show)

data Section a
  = Section
      { heading :: a,
        content :: SectionContent a
      }
  | BibTeXPublications
      { pubsHeading :: a,
        citeKeys :: [Text]
      }
  deriving (Functor, Foldable, Traversable, Generic, Eq, Show)

data SectionContent a
  = Paragraph a
  | Work [Job a]
  | Volunteering [Volunteer a]
  | Skills [Skill a]
  | Education [Study a]
  | Awards [Award a]
  | Publications [Publication a]
  | Languages [Language]
  | Interests [Interest a]
  deriving (Functor, Foldable, Traversable, Generic, Eq, Show)

data Job a
  = Job
      { position :: Text,
        company :: Text,
        jobStartDate :: Date,
        jobEndDate :: Maybe Date,
        jobLocation :: Maybe Text,
        companyUrl :: Maybe Text,
        jobSummary :: Maybe a
      }
  deriving (Functor, Foldable, Traversable, Generic, Eq, Show)

data Volunteer a
  = Volunteer
      { volunteerPosition :: Text,
        organization :: Text,
        volunteerStartDate :: Date,
        volunteerEndDate :: Maybe Date,
        volunteerLocation :: Maybe Text,
        organizationUrl :: Maybe Text,
        volunteerSummary :: Maybe a
      }
  deriving (Functor, Foldable, Traversable, Generic, Eq, Show)

data Study a
  = Study
      { studyType :: Text,
        area :: Text,
        institution :: Text,
        studyStartDate :: Date,
        studyEndDate :: Maybe Date,
        studyLocation :: Maybe Text,
        institutionUrl :: Maybe Text,
        gpa :: Maybe Text,
        courses :: [a],
        studySummary :: Maybe a
      }
  deriving (Functor, Foldable, Traversable, Generic, Eq, Show)

data Skill a
  = Skill
      { skillArea :: a,
        skillKeywords :: [a],
        skillSummary :: Maybe a
      }
  deriving (Functor, Foldable, Traversable, Generic, Eq, Show)

data Award a
  = Award
      { awardTitle :: Text,
        awardDate :: Date,
        awarder :: Text,
        awardSummary :: Maybe a
      }
  deriving (Functor, Foldable, Traversable, Generic, Eq, Show)

data Publication a
  = Publication
      { publicationTitle :: Text,
        publisher :: Text,
        publicationDate :: Date,
        publicationUrl :: Maybe Text,
        publicationSummary :: Maybe a
      }
  deriving (Functor, Foldable, Traversable, Generic, Eq, Show)

data Interest a
  = Interest
      { interest :: Text,
        keywords :: [a]
      }
  deriving (Functor, Foldable, Traversable, Generic, Eq, Show)

instance FromDhall a => FromDhall (Award a)

instance FromDhall a => FromDhall (Resume a)

instance FromDhall a => FromDhall (Section a)

instance FromDhall a => FromDhall (Interest a)

instance FromDhall a => FromDhall (Job a)

instance FromDhall a => FromDhall (Publication a)

instance FromDhall a => FromDhall (SectionContent a)

instance FromDhall a => FromDhall (Skill a)

instance FromDhall a => FromDhall (Study a)

instance FromDhall a => FromDhall (Volunteer a)
