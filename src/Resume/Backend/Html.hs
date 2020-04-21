{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Resume.Backend.Html
  ( compileHtml
  , def
  )
where

import           Control.Monad                  ( (>=>) )
import           Data.Default
import           Data.Maybe
import           Data.Text
import qualified Data.Text.Lazy                as TL
import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html5              as H
import           Text.Blaze.Html5.Attributes   as A
import           Text.Pandoc                    ( PandocError )
import qualified Text.Pandoc                   as P

import           Resume.Types                  as R


data HtmlBackendOptions = HtmlBackendOptions
  { cssPaths :: [Text]
  }

instance Default HtmlBackendOptions where
  def = HtmlBackendOptions
    { cssPaths = [ "node_modules/normalize.css/normalize.css"
                 , "node_modules/font-awesome.css/css/font-awesome.css"
                 , "style.css"
                 ]
    }

compileHtml :: HtmlBackendOptions -> Resume Markdown -> Either PandocError Text
compileHtml opts r =
  TL.toStrict . renderHtml . mkResume opts <$> traverse fromMarkdown r
 where
  fromMarkdown =
    P.runPure . (P.readMarkdown def >=> P.writeHtml5 def) . unMarkdown


mkResume :: HtmlBackendOptions -> Resume Html -> Html
mkResume opts Resume {..} =
  docTypeHtml
    $  H.head
         (  (meta ! charset "utf-8")
         <> H.title
              (toHtml
                (let r = R.name basics in firstName r <> " " <> lastName r)
              )
         <> mconcat
              (fmap
                (\path -> link ! rel "stylesheet" ! A.type_ "text/css" ! href
                  (textValue path)
                )
                (cssPaths opts)
              )
         )
    <> body
         (  h1
             (let r = R.name basics
              in  toHtml $ firstName r <> " " <> lastName r
             )
         <> (  H.div
            !  class_ "contact-info"
            $  mkContactItem (email basics)
                             (Just $ "mailto:" <> email basics)
                             (Just "fa fa-envelope")
            <> maybe mempty
                     (\t -> mkContactItem t (Just t) (Just "fa fa-globe"))
                     (homepage profiles)
            <> maybe
                 mempty
                 (mkSocialItem "//www.linkedin.com/in" "fa fa-linkedin")
                 (linkedin profiles)
            <> maybe mempty
                     (mkSocialItem "//www.twitter.com/" "fa fa-twitter")
                     (twitter profiles)
            <> maybe mempty
                     (mkSocialItem "//www.github.com/" "fa fa-github")
                     (github profiles)
            )
         <> mconcat (fmap toHtml sections)
         )

mkSocialItem :: Text -> Text -> Social -> Html
mkSocialItem baseUrl iconClass Social {..} = mkContactItem user
                                                           (Just url_)
                                                           (Just iconClass)
  where url_ = fromMaybe (baseUrl <> user) url

mkContactItem :: Text -> Maybe Text -> Maybe Text -> Html
mkContactItem t url iconClass =
  li
    $  maybe mempty (\ic -> i ! class_ (textValue ic) $ mempty) iconClass
    <> (a ! maybe mempty (href . textValue) url $ toHtml t)

entries :: Html -> Html
entries = H.div ! class_ "resume-entries"

mkEntry :: Html -> Html -> Html
mkEntry left right =
  (H.div ! class_ "resume-hint" $ left)
    <> (H.div ! class_ "resume-description" $ right)

instance ToMarkup a => ToMarkup (Section a) where
  toMarkup Section { content = content_, ..} =
    h2 (toHtml heading) <> toHtml content_

instance ToMarkup a => ToMarkup (SectionContent a) where
  toMarkup = \case
    Paragraph    _  -> error "not implemented"
    Work         xs -> entries . mconcat $ fmap toHtml xs
    Volunteering _  -> error "not implemented"
    Skills       xs -> entries . mconcat $ fmap toHtml xs
    Education    xs -> entries . mconcat $ fmap toHtml xs
    Awards       _  -> error "not implemented"
    Publications _  -> error "not implemented"
    Languages    _  -> error "not implemented"
    Interests    _  -> error "not implemented"

instance ToMarkup Date where
  toMarkup Date {..} = toMarkup month <> "/" <> toMarkup year

instance ToMarkup a => ToMarkup (Job a) where
  toMarkup Job {..} =
    mkEntry
        (toHtml jobStartDate <> maybe mempty ((" - " <>) . toHtml) jobEndDate)
      $  (  H.div
         !  class_ (textValue "resume-entry-heading")
         $  (H.span ! class_ (textValue "resume-entry-title") $ toHtml position)
         <> (H.span ! class_ (textValue "resume-entry-company") $ toHtml
              company
            )
         <> maybe
              mempty
              (\t ->
                H.span ! class_ (textValue "resume-entry-location") $ toHtml t
              )
              jobLocation
         )
      <> maybe mempty
               (\t -> H.div ! class_ "resume-entry-summary" $ toHtml t)
               jobSummary

instance ToMarkup a => ToMarkup (Skill a) where
  toMarkup Skill {..} =
    mkEntry (toHtml skillArea) $ maybe mempty toHtml skillSummary

instance ToMarkup a => ToMarkup (Study a) where
  toMarkup Study {..} =
    mkEntry
        (  toHtml studyStartDate
        <> maybe mempty ((" - " <>) . toHtml) studyEndDate
        )
      $  (  H.div
         !  class_ (textValue "resume-entry-heading")
         $  (H.span ! class_ (textValue "resume-entry-title") $ toHtml
              (studyType <> ", " <> area)
            )
         <> (H.span ! class_ (textValue "resume-entry-company") $ toHtml
              institution
            )
         <> maybe
              mempty
              (\t ->
                H.span ! class_ (textValue "resume-entry-location") $ toHtml t
              )
              studyLocation
         )
      <> maybe mempty
               (\t -> H.div ! class_ "resume-entry-summary" $ toHtml t)
               studySummary
