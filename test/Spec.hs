{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import           Data.Text
import           Test.Hspec
import           Text.RawString.QQ

import           CV                             ( parseCV )
import           CV.Types

input :: Text
input = [r|
let types = (./dhall/types/package.dhall).mkTypes Text

in  { name = { first = "first", last = "last" }
    , headline = Some "headline"
    , city = "city"
    , contact =
        { email = "email"
        , linkedin = "linkedin"
        , twitter = Some "twitter"
        , phone = "111-111-1111"
        }
    , sections =
      [ { heading = "Experience"
        , content =
            types.SectionContent.Experiences
              [ { title = "title"
                , organization = "organization"
                , location = "location"
                , startMonth = { year = 2020, month = 1 }
                , endMonth = Some { year = 2020, month = 1 }
                , items = [ "foo", "bar", "baz" ]
                }
              ]
        }
      , { heading = "Skills"
        , content =
            types.SectionContent.Skills
              [ { category = "category", keywords = [ "fizz", "bum" ] } ]
        }
      ]
    }
|]

parsed :: CV Text
parsed = CV
  { name     = Name "first" "last"
  , headline = Just "headline"
  , city     = "city"
  , contact  = ContactInfo { email    = "email"
                           , linkedin = "linkedin"
                           , twitter  = Just "twitter"
                           , phone    = "111-111-1111"
                           }
  , sections =
    [ CVSection
      { heading = "Experience"
      , content = Experiences
                    [ Experience { title        = "title"
                                 , organization = "organization"
                                 , location     = "location"
                                 , startMonth   = CVDate 2020 1
                                 , endMonth     = Just $ CVDate 2020 1
                                 , items        = ["foo", "bar", "baz"]
                                 }
                    ]
      }
    , CVSection
      { heading = "Skills"
      , content = Skills
        [SkillCategory { category = "category", keywords = ["fizz", "bum"] }]
      }
    ]
  }

main :: IO ()
main = hspec $ do
  describe "Frontend" $ do
    it "should parse simple example" $ do
      parseCV input `shouldReturn` parsed

