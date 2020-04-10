{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

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

-- | Used to wrap text that should be interpreted as Markdown
newtype Markdown = Markdown { unMarkdown :: Text } deriving (Eq, Show)

data Basics = Basics
  { name     :: Name
  , email    :: Text
  , location :: Location
  , phone    :: Maybe Text
  } deriving (Generic, FromDhall, Eq, Show)

data Name = Name
  { firstName :: Text
  , lastName :: Text
  } deriving (Generic, FromDhall, Eq, Show)

data Location
  = StreetAddress
    { address    :: Text
    , city       :: Text
    , postalCode :: Text
    , country    :: Maybe Text
    }
  | Region Text
  deriving (Generic, FromDhall, Eq, Show)

data Profiles = Profiles
 {  homepage :: Maybe Text
  , linkedin :: Maybe Social
  , github   :: Maybe Social
  , twitter  :: Maybe Social
 } deriving (Generic, FromDhall, Eq, Show)

data CVDate = CVDate
  { year  :: Natural
  , month :: Natural
  } deriving (Generic, FromDhall, Eq, Show)

data Language = Language
  { language :: Text
  , fluency  :: Text
  } deriving (Generic, FromDhall, Eq, Show)

data Social = Social
  { user :: Text
  , url  :: Maybe Text
  } deriving (Generic, FromDhall, Eq, Show)

-- The type parameter 'a' in the types below refers to underlying text
-- representation. The input type is always 'CV Text'.  This might be
-- converted to a different representation before rendering depending
-- on the desired output format.

data CV a = CV
  { basics   :: Basics
  , profiles :: Profiles
  , headline :: Maybe a
  , sections :: [CVSection a]
  } deriving (Functor, Foldable, Traversable, Generic, Eq, Show)

data CVSection a = CVSection
  { heading :: Text
  , content :: SectionContent a
  } deriving (Functor, Foldable, Traversable, Generic, Eq, Show)

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

data Job a = Job
  { position      :: Text
  , company       :: Text
  , jobStartDate  :: CVDate
  , jobEndDate    :: Maybe CVDate
  , jobLocation   :: Maybe Text
  , companyUrl    :: Maybe Text
  , jobSummary    :: Maybe a
  } deriving (Functor, Foldable, Traversable, Generic, Eq, Show)

data Volunteer a = Volunteer
  { volunteerPosition   :: Text
  , organization        :: Text
  , volunteerStartDate  :: CVDate
  , volunteerEndDate    :: Maybe CVDate
  , volunteerLocation   :: Maybe Text
  , organizationUrl     :: Maybe Text
  , volunteerSummary    :: Maybe a
  } deriving (Functor, Foldable, Traversable, Generic, Eq, Show)

data Study a = Study
  { studyType      :: Text
  , area           :: Text
  , institution    :: Text
  , studyStartDate :: CVDate
  , studyEndDate   :: Maybe CVDate
  , studyLocation  :: Maybe Text
  , institutionUrl :: Maybe Text
  , gpa            :: Maybe Text
  , courses        :: [a]
  , studySummary   :: Maybe a
  } deriving (Functor, Foldable, Traversable, Generic, Eq, Show)

data Skill a = Skill
  { skillArea     :: a
  , skillKeywords :: [a]
  , skillSummary  :: Maybe a
  } deriving (Functor, Foldable, Traversable, Generic, Eq, Show)

data Award a = Award
  { awardTitle   :: Text
  , awardDate    :: CVDate
  , awarder      :: Text
  , awardSummary :: Maybe a
  } deriving (Functor, Foldable, Traversable, Generic, Eq, Show)

data Publication a = Publication
  { publicationTitle   :: Text
  , publisher          :: Text
  , publicationDate    :: CVDate
  , publicationUrl     :: Maybe Text
  , publicationSummary :: Maybe a
  } deriving (Functor, Foldable, Traversable, Generic, Eq, Show)

data Interest a = Interest
  { interest :: Text
  , keywords :: [a]
  } deriving (Functor, Foldable, Traversable, Generic, Eq, Show)


instance FromDhall a => FromDhall (Award a)
instance FromDhall a => FromDhall (CV a)
instance FromDhall a => FromDhall (CVSection a)
instance FromDhall a => FromDhall (Interest a)
instance FromDhall a => FromDhall (Job a)
instance FromDhall a => FromDhall (Publication a)
instance FromDhall a => FromDhall (SectionContent a)
instance FromDhall a => FromDhall (Skill a)
instance FromDhall a => FromDhall (Study a)
instance FromDhall a => FromDhall (Volunteer a)
