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
      [ { heading = Some ""
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
      , { heading = Some ""
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
      , { heading = Some ""
        , content =
            types.SectionContent.Skills
              [ { skillArea = ""
                , skillKeywords = [ "" ]
                , skillSummary = Some ""
                }
              ]
        }
      , { heading = Some ""
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
      , { heading = Some ""
        , content =
            types.SectionContent.Awards
              [ { awardTitle = ""
                , awardDate = { year = 1, month = 1 }
                , awarder = ""
                , awardSummary = Some ""
                }
              ]
        }
      , { heading = Some ""
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
      , { heading = Some ""
        , content =
            types.SectionContent.BibTeXPublications [""]
        }
      , { heading = Some ""
        , content =
            types.SectionContent.Interests
              [ { interest = "", keywords = [ "" ] } ]
        }
      , { heading = Some ""
        , content =
            types.SectionContent.Languages [ { language = "", fluency = "" } ]
        }
      ]
    }
|]

exampleResume :: Resume Markdown
exampleResume =
  Markdown
    <$> Resume
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
              { heading = Just "",
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
              { heading = Just "",
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
              { heading = Just "",
                content =
                  Skills
                    [Skill {skillArea = "", skillKeywords = [""], skillSummary = Just ""}]
              },
            Section
              { heading = Just "",
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
              { heading = Just "",
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
              { heading = Just "",
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
            Section
              { heading = Just "",
                content = BibTeXPublications [""]
              },
            Section
              { heading = Just "",
                content = Interests [Interest {interest = "", keywords = [""]}]
              },
            Section
              { heading = Just "",
                content = Languages [Language {language = "", fluency = ""}]
              }
          ]
      }

main :: IO ()
main = hspec $ do
  describe "Frontend" $ do
    it "should parse simple example" $ do
      parseResume defaultInputSettings exampleInput `shouldReturn` exampleResume
