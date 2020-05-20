{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Resume.Backend.Html
  ( renderHtml,
    renderHtmlBody,
    renderHtmlStyles,
    renderText,
    def,
  )
where

import Clay ((#), (<?), (?), Css, em, pc, px)
import qualified Clay as C
import Control.Monad.Reader
import Data.Default
import Data.Foldable
import Data.Maybe
import Data.Text
import qualified Data.Text.Lazy as TL
import Lucid hiding (renderText)
import qualified Lucid
import Lucid.Base (commuteHtmlT)
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
        style = Just defaultStyle
      }

defaultStyle :: Css
defaultStyle =
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

mkRender ::
  (Resume Text -> HtmlM ()) ->
  HtmlBackendOptions ->
  Resume Markdown ->
  Either PandocError (Html ())
mkRender part opts =
  fmap
    ( flip runReader opts
        . commuteHtmlT
        . part
    )
    . traverse fromMarkdown

renderHtml ::
  HtmlBackendOptions ->
  Resume Markdown ->
  Either PandocError (Html ())
renderHtml = mkRender resume

renderHtmlBody ::
  HtmlBackendOptions ->
  Resume Markdown ->
  Either PandocError (Html ())
renderHtmlBody = mkRender resumeBody

renderHtmlStyles :: HtmlBackendOptions -> Html ()
renderHtmlStyles opts = flip runReader opts . commuteHtmlT $ resumeStyles

renderText ::
  HtmlBackendOptions ->
  Resume Markdown ->
  Either PandocError Text
renderText opts = fmap (TL.toStrict . Lucid.renderText) . renderHtml opts

fromMarkdown :: Markdown -> Either PandocError Text
fromMarkdown =
  P.runPure
    . (P.readMarkdown def >=> P.writeHtml5String def)
    . unMarkdown

type HtmlM = HtmlT (Reader HtmlBackendOptions)

resume :: Resume Text -> HtmlM ()
resume r = html_ [lang_ "en"] $ do
  head_ $ resumeHead r
  body_ $ resumeBody r

resumeHead :: Resume Text -> HtmlM ()
resumeHead Resume {..} = do
  meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
  maybe "Resume" (\Name {..} -> title_ . toHtml $ firstName <> " " <> lastName) (name basics)
  resumeStyles

resumeStyles :: HtmlM ()
resumeStyles = do
  opts <- ask
  mconcat $
    ( \path ->
        link_ [rel_ "stylesheet", type_ "text/css", href_ path]
    )
      <$> cssUrls opts
  foldMap (style_ [type_ "text/css"] . C.render) $ style opts

resumeBody :: Resume Text -> HtmlM ()
resumeBody Resume {..} = do
  foldMap (\Name {..} -> h1_ . toHtml $ firstName <> " " <> lastName) (R.name basics)
  foldMap (h2_ . toHtmlRaw) headline
  div_ [class_ "contact-info"]
    $ ul_
    $ do
      contactItem
        (email basics)
        (Just $ "mailto:" <> email basics)
        (Just "fa fa-envelope")
      foldMap
        ( \Link {..} ->
            contactItem
              (fromMaybe url label)
              (Just url)
              (Just "fa fa-globe")
        )
        (homepage profiles)
      foldMap
        (socialItem "//www.linkedin.com/in" "fa fa-linkedin")
        (linkedin profiles)
      foldMap
        (socialItem "//www.twitter.com/" "fa fa-twitter")
        (twitter profiles)
      foldMap
        (socialItem "//www.github.com/" "fa fa-github")
        (github profiles)
  mapM_ section sections

socialItem :: Text -> Text -> Social -> HtmlM ()
socialItem baseUrl iconClass Social {..} =
  contactItem
    user
    (Just url_)
    (Just iconClass)
  where
    url_ = fromMaybe (baseUrl <> user) profileUrl

contactItem :: Text -> Maybe Text -> Maybe Text -> HtmlM ()
contactItem t url iconClass =
  li_
    $ a_ (toList $ fmap href_ url)
    $ do
      foldMap (\ic -> i_ [class_ ic] mempty) iconClass
      toHtml t

entry :: Monad m => HtmlT m () -> HtmlT m () -> HtmlT m ()
entry left right = div_ [class_ "resume-entry"] $ do
  div_ [class_ "resume-hint"] left
  div_ [class_ "resume-description"] right

section :: ToHtml a => Section a -> HtmlM ()
section Section {..} = do
  h2_ $ toHtml heading
  sectionContent content

sectionContent :: ToHtml a => SectionContent a -> HtmlM ()
sectionContent = \case
  Paragraph _ -> error "not implemented"
  Work xs -> mconcat $ fmap job xs
  Volunteering _ -> error "not implemented"
  Skills xs -> mconcat $ fmap skill xs
  Education xs -> mconcat $ fmap study xs
  Awards _ -> error "not implemented"
  Publications _ -> error "not implemented"
  Languages _ -> error "not implemented"
  Interests _ -> error "not implemented"

date :: Date -> HtmlM ()
date Date {..} = toHtml (show month <> "/" <> show year)

job :: ToHtml a => Job a -> HtmlM ()
job Job {..} =
  entry
    (date jobStartDate <> foldMap ((" - " <>) . date) jobEndDate)
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

skill :: ToHtml a => Skill a -> HtmlM ()
skill Skill {..} =
  entry (toHtmlRaw skillArea)
    $ div_ [class_ "resume-entry-summary"]
    $ foldMap toHtmlRaw skillSummary

study :: ToHtml a => Study a -> HtmlM ()
study Study {..} =
  entry
    ( date studyStartDate <> foldMap ((" - " <>) . date) studyEndDate
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
