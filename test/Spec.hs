{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Text
import Resume (defaultInputSettings, parseResume)
import Resume.Types
import Test.Hspec
import Text.RawString.QQ

exampleInput :: Text
exampleInput =
  [r|
let types = (./dhall/types/package.dhall).mkTypes Text

in  { basics =
        { name = Some { firstName = "", lastName = "" }
        , email = ""
        , phone = Some ""
        , location =
            types.Location.StreetAddress
              { address = "", city = "", postalCode = "", country = Some "" }
        }
    , profiles =
        { homepage = Some { url = "", label = Some "" }
        , linkedin = Some { user = "", profileUrl = Some "" }
        , github = Some { user = "", profileUrl = Some "" }
        , twitter = Some { user = "", profileUrl = Some "" }
        }
    , headline = Some ""
    , sections =
      [ types.Section.Section
          { heading = ""
          , content =
              types.SectionContent.Work
                [ { position = ""
                  , company = ""
                  , jobStartDate = { year = 1, month = 1 }
                  , jobEndDate = Some { year = 1, month = 1 }
                  , jobLocation = Some ""
                  , companyUrl = Some ""
                  , jobSummary = Some ""
                  }
                ]
          }
      , types.Section.Section
          { heading = ""
          , content =
              types.SectionContent.Volunteering
                [ { volunteerPosition = ""
                  , organization = ""
                  , volunteerStartDate = { year = 1, month = 1 }
                  , volunteerEndDate = Some { year = 1, month = 1 }
                  , volunteerLocation = Some ""
                  , organizationUrl = Some ""
                  , volunteerSummary = Some ""
                  }
                ]
          }
      , types.Section.Section
          { heading = ""
          , content =
              types.SectionContent.Skills
                [ { skillArea = ""
                  , skillKeywords = [ "" ]
                  , skillSummary = Some ""
                  }
                ]
          }
      , types.Section.Section
          { heading = ""
          , content =
              types.SectionContent.Education
                [ { studyType = ""
                  , area = ""
                  , institution = ""
                  , studyStartDate = { year = 1, month = 1 }
                  , studyEndDate = Some { year = 1, month = 1 }
                  , studyLocation = Some ""
                  , institutionUrl = Some ""
                  , gpa = Some ""
                  , courses = [ "" ]
                  , studySummary = Some ""
                  }
                ]
          }
      , types.Section.Section
          { heading = ""
          , content =
              types.SectionContent.Awards
                [ { awardTitle = ""
                  , awardDate = { year = 1, month = 1 }
                  , awarder = ""
                  , awardSummary = Some ""
                  }
                ]
          }
      , types.Section.Section
          { heading = ""
          , content =
              types.SectionContent.Interests
                [ { interest = "", keywords = [ "" ] } ]
          }
      , types.Section.Section
          { heading = ""
          , content =
              types.SectionContent.Languages [ { language = "", fluency = "" } ]
          }
      , types.Section.Section
          { heading = ""
          , content =
              types.SectionContent.Publications
                [ { publicationTitle = ""
                  , publisher = ""
                  , publicationDate = { year = 1, month = 1 }
                  , publicationUrl = Some ""
                  , publicationSummary = Some ""
                  }
                ]
          }
      , types.Section.BibTeXPublications { pubsHeading = "", citeKeys = [ "" ] }
      ]
    }
|]

exampleResume :: Resume Text
exampleResume =
  Resume
    { basics =
        Basics
          { name = Just $ Name "" "",
            email = "",
            phone = Just "",
            location =
              StreetAddress
                { city = "",
                  address = "",
                  postalCode = "",
                  country = Just ""
                }
          },
      profiles =
        Profiles
          { homepage = Just $ Link "" $ Just "",
            linkedin = Just $ Social "" $ Just "",
            github = Just $ Social "" $ Just "",
            twitter = Just $ Social "" $ Just ""
          },
      headline = Just "",
      sections =
        [ Section
            { heading = "",
              content =
                Work
                  [ Job
                      { position = "",
                        company = "",
                        jobStartDate = Date 1 1,
                        jobEndDate = Just $ Date 1 1,
                        jobLocation = Just "",
                        companyUrl = Just "",
                        jobSummary = Just ""
                      }
                  ]
            },
          Section
            { heading = "",
              content =
                Volunteering
                  [ Volunteer
                      { volunteerPosition = "",
                        organization = "",
                        volunteerStartDate = Date 1 1,
                        volunteerEndDate = Just $ Date 1 1,
                        volunteerLocation = Just "",
                        organizationUrl = Just "",
                        volunteerSummary = Just ""
                      }
                  ]
            },
          Section
            { heading = "",
              content =
                Skills
                  [Skill {skillArea = "", skillKeywords = [""], skillSummary = Just ""}]
            },
          Section
            { heading = "",
              content =
                Education
                  [ Study
                      { studyType = "",
                        area = "",
                        institution = "",
                        studyStartDate = Date 1 1,
                        studyEndDate = Just $ Date 1 1,
                        studyLocation = Just "",
                        institutionUrl = Just "",
                        gpa = Just "",
                        courses = [""],
                        studySummary = Just ""
                      }
                  ]
            },
          Section
            { heading = "",
              content =
                Awards
                  [ Award
                      { awardTitle = "",
                        awardDate = Date 1 1,
                        awarder = "",
                        awardSummary = Just ""
                      }
                  ]
            },
          Section
            { heading = "",
              content = Interests [Interest {interest = "", keywords = [""]}]
            },
          Section
            { heading = "",
              content = Languages [Language {language = "", fluency = ""}]
            },
          Section
            { heading = "",
              content =
                Publications
                  [ Publication
                      { publicationTitle = "",
                        publisher = "",
                        publicationDate = Date 1 1,
                        publicationUrl = Just "",
                        publicationSummary = Just ""
                      }
                  ]
            },
          BibTeXPublications {pubsHeading = "", citeKeys = [""]}
        ]
    }

main :: IO ()
main = hspec $ do
  describe "Frontend" $ do
    it "should parse simple example" $ do
      parseResume defaultInputSettings exampleInput `shouldReturn` exampleResume
