{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Resume.Backend.Html
  ( compileHtml,
    def,
  )
where

import Clay ((#), (<?), (?), Css, em, pc, px)
import qualified Clay as C
import Control.Monad ((>=>))
import Data.Default
import Data.Foldable
import Data.Maybe
import Data.Text
import qualified Data.Text.Lazy as TL
import Lucid
import Resume.Types as R
import Text.Pandoc (PandocError)
import qualified Text.Pandoc as P

data HtmlBackendOptions
  = HtmlBackendOptions
      { cssUrls :: [Text],
        style :: Maybe Css
      }

instance Default HtmlBackendOptions where
  def =
    HtmlBackendOptions
      { cssUrls =
          [ "https://cdnjs.cloudflare.com/ajax/libs/normalize/8.0.1/normalize.min.css",
            "https://stackpath.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
          ],
        style = Just $
          C.body ? do
            C.margin (em 4) (pc 20) (em 1) (pc 20)
            C.div # ".contact-info" <? C.ul <? do
              C.paddingLeft $ px 0
              C.listStyleType C.none
              C.li <? do
                C.display C.inline
                C.paddingRight $ em 1
                C.a <? C.i <? C.paddingRight (px 5)
            C.div # ".resume-entry" ? do
              C.display C.flex
              C.flexDirection C.row
              C.div # ".resume-hint" <? do
                C.flexBasis $ px 100
                C.flexGrow 0
                C.flexShrink 0
              C.div # ".resume-description" <? do
                C.width (pc 100)
                C.div # ".resume-entry-heading" <? do
                  C.span <? C.paddingRight (em 1)
                  C.span # ".resume-entry-title" <? C.fontWeight C.bold
                  C.span # ".resume-entry-company" <? C.fontStyle C.italic
                C.div # ".resume-entry-summary" <? C.ul <? C.paddingLeft (em 1)
      }

compileHtml :: HtmlBackendOptions -> Resume Markdown -> Either PandocError Text
compileHtml opts r = TL.toStrict . renderText . mkResume opts <$> traverse fromMarkdown r
  where
    fromMarkdown =
      P.runPure . (P.readMarkdown def >=> P.writeHtml5String def) . unMarkdown

mkResume :: HtmlBackendOptions -> Resume Text -> Html ()
mkResume opts Resume {..} =
  html_ [lang_ "en"] $ do
    head_ $ do
      meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
      title_ $ toHtml $ let r = R.name basics in firstName r <> " " <> lastName r
      mconcat $
        ( \path ->
            link_ [rel_ "stylesheet", type_ "text/css", href_ path]
        )
          <$> cssUrls opts
      foldMap (style_ [type_ "text/css"] . C.render) $ style opts
    body_ $ do
      h1_
        ( let r = R.name basics
           in toHtml $ firstName r <> " " <> lastName r
        )
      div_ [class_ "contact-info"]
        $ ul_
        $ do
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
  li_
    $ a_ (toList $ fmap href_ url)
    $ do
      foldMap (\ic -> i_ [class_ ic] mempty) iconClass
      toHtml t

mkEntry :: Html () -> Html () -> Html ()
mkEntry left right = div_ [class_ "resume-entry"] $ do
  div_ [class_ "resume-hint"] left
  div_ [class_ "resume-description"] right

mkSection :: ToHtml a => Section a -> Html ()
mkSection Section {..} = do
  h2_ $ toHtml heading
  mkSectionContent content

mkSectionContent :: ToHtml a => SectionContent a -> Html ()
mkSectionContent = \case
  Paragraph _ -> error "not implemented"
  Work xs -> mconcat $ fmap mkJob xs
  Volunteering _ -> error "not implemented"
  Skills xs -> mconcat $ fmap mkSkill xs
  Education xs -> mconcat $ fmap mkStudy xs
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
  mkEntry (toHtmlRaw skillArea)
    $ div_ [class_ "resume-entry-summary"]
    $ foldMap toHtmlRaw skillSummary

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
