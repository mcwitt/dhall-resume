{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Resume.Backend.Html
  ( compileHtml,
    def,
  )
where

import Control.Monad ((>=>))
import Data.Default
import Data.Maybe
import Data.Text
import qualified Data.Text.Lazy as TL
import Lucid
import Resume.Types as R
import Text.Pandoc (PandocError)
import qualified Text.Pandoc as P

data HtmlBackendOptions
  = HtmlBackendOptions
      { cssPaths :: [Text]
      }

instance Default HtmlBackendOptions where
  def =
    HtmlBackendOptions
      { cssPaths =
          [ "node_modules/normalize.css/normalize.css",
            "node_modules/font-awesome.css/css/font-awesome.css",
            "style.css"
          ]
      }

compileHtml :: HtmlBackendOptions -> Resume Markdown -> Either PandocError Text
compileHtml opts r =
  TL.toStrict . renderText . mkResume opts <$> traverse fromMarkdown r
  where
    fromMarkdown =
      P.runPure . (P.readMarkdown def >=> P.writeHtml5String def) . unMarkdown

mkResume :: HtmlBackendOptions -> Resume Text -> Html ()
mkResume opts Resume {..} =
  html_ [lang_ "en"] $ do
    head_ $ do
      meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
      title_ (toHtml (let r = R.name basics in firstName r <> " " <> lastName r))
      mconcat $
        ( \path ->
            link_ [rel_ "stylesheet", type_ "text/css", href_ path]
        )
          <$> cssPaths opts
    body_ $ do
      h1_
        ( let r = R.name basics
           in toHtml $ firstName r <> " " <> lastName r
        )
      div_ [class_ "contact-info"] $ do
        mkContactItem
          (email basics)
          (Just $ "mailto:" <> email basics)
          (Just "fa fa-envelope")
        foldMap
          (\t -> mkContactItem t (Just t) (Just "fa fa-globe"))
          (homepage profiles)
        foldMap
          (mkSocialItem "//www.linkedin.com/in" "fa fa-linkedin")
          (linkedin profiles)
        foldMap
          (mkSocialItem "//www.twitter.com/" "fa fa-twitter")
          (twitter profiles)
        foldMap
          (mkSocialItem "//www.github.com/" "fa fa-github")
          (github profiles)
        mconcat (fmap mkSection sections)

mkSocialItem :: Text -> Text -> Social -> Html ()
mkSocialItem baseUrl iconClass Social {..} =
  mkContactItem
    user
    (Just url_)
    (Just iconClass)
  where
    url_ = fromMaybe (baseUrl <> user) url

mkContactItem :: Text -> Maybe Text -> Maybe Text -> Html ()
mkContactItem t url iconClass =
  li_ $ do
    foldMap (\ic -> i_ [class_ ic] mempty) iconClass
    a_ (catMaybes [fmap href_ url]) $ toHtml t

entries :: Html () -> Html ()
entries = div_ [class_ "resume-entries"]

mkEntry :: Html () -> Html () -> Html ()
mkEntry left right = do
  div_ [class_ "resume-hint"] left
  div_ [class_ "resume-description"] right

mkSection :: ToHtml a => Section a -> Html ()
mkSection Section {..} = do
  h2_ $ toHtml heading
  mkSectionContent content

mkSectionContent :: ToHtml a => SectionContent a -> Html ()
mkSectionContent = \case
  Paragraph _ -> error "not implemented"
  Work xs -> entries . mconcat $ fmap mkJob xs
  Volunteering _ -> error "not implemented"
  Skills xs -> entries . mconcat $ fmap mkSkill xs
  Education xs -> entries . mconcat $ fmap mkStudy xs
  Awards _ -> error "not implemented"
  Publications _ -> error "not implemented"
  Languages _ -> error "not implemented"
  Interests _ -> error "not implemented"

mkDate :: Date -> Html ()
mkDate Date {..} = toHtml (show month <> "/" <> show year)

mkJob :: ToHtml a => Job a -> Html ()
mkJob Job {..} =
  mkEntry
    (mkDate jobStartDate <> foldMap ((" - " <>) . mkDate) jobEndDate)
    $ do
      div_ [class_ "resume-entry-heading"] $ do
        span_ [class_ "resume-entry-title"] $ toHtml position
        span_ [class_ "resume-entry-company"] $
          toHtml
            company
        foldMap
          ( span_ [class_ "resume-entry-location"] . toHtml
          )
          jobLocation
      foldMap
        (div_ [class_ "resume-entry-summary"] . toHtmlRaw)
        jobSummary

mkSkill :: ToHtml a => Skill a -> Html ()
mkSkill Skill {..} =
  mkEntry (toHtmlRaw skillArea) $ foldMap toHtmlRaw skillSummary

mkStudy :: ToHtml a => Study a -> Html ()
mkStudy Study {..} =
  mkEntry
    ( mkDate studyStartDate <> foldMap ((" - " <>) . mkDate) studyEndDate
    )
    $ do
      div_ [class_ "resume-entry-heading"] $ do
        span_ [class_ "resume-entry-title"] $
          toHtml
            (studyType <> ", " <> area)
        span_ [class_ "resume-entry-company"] $
          toHtml
            institution
        foldMap
          ( span_ [class_ "resume-entry-location"] . toHtml
          )
          studyLocation
      foldMap
        (div_ [class_ "resume-entry-summary"] . toHtmlRaw)
        studySummary
