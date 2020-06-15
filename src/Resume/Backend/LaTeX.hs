{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Resume.Backend.LaTeX
  ( LaTeXOptions (..),
    renderText,
  )
where

import Control.Monad.Reader
import Data.Default
import Data.Maybe
import qualified Data.Text as T
import Resume.Types as R
import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.Geometry
import Text.LaTeX.Packages.Inputenc
import Text.Pandoc (Pandoc, PandocError)
import qualified Text.Pandoc as P
import Text.Printf

data LaTeXOptions
  = LaTeXOptions
      { modernCvColor :: Text,
        modernCvStyle :: Text,
        bibFile :: Maybe FilePath,
        hintsColumnWidth :: Maybe Text
      }

instance Default LaTeXOptions where
  def =
    LaTeXOptions
      { modernCvColor = "blue",
        modernCvStyle = "casual",
        bibFile = Nothing,
        hintsColumnWidth = Nothing
      }

-- | Render Resume with Markdown-formatted text as LaTeX
renderText :: LaTeXOptions -> Resume Pandoc -> Either PandocError Text
renderText opts =
  fmap (render . flip runReader opts . execLaTeXT . resume)
    . traverse (P.runPure . P.writeLaTeX def)

type LaTeXReader = LaTeXT (Reader LaTeXOptions)

resume :: Resume Text -> LaTeXReader ()
resume Resume {..} = do
  documentclass [FontSize (Pt 11), Paper A4] "moderncv"
  usepackage [raw "scale=0.8"] geometry
  usepackage [utf8] inputenc
  pandocHeader
  lift (asks modernCvStyle) >>= comm1 "moderncvstyle" . raw
  lift (asks modernCvColor) >>= comm1 "moderncvcolor" . raw
  lift (asks hintsColumnWidth) >>= foldMap (comm2 "setlength" (commS "hintscolumnwidth") . raw)
  foldMap (title . raw) headline
  foldMap
    (\Name {..} -> comm2 "name" (raw firstName) (raw lastName))
    (name basics)
  comm1 "email" $ raw $ email basics
  foldMap (\R.Link {url = url} -> comm1 "homepage" $ raw url) $
    homepage profiles
  foldMap (mkSocial "linkedin") $ linkedin profiles
  foldMap (mkSocial "twitter") $ twitter profiles
  foldMap (mkSocial "github") $ github profiles
  case location basics of
    StreetAddress {..} ->
      comm3
        "address"
        (raw address)
        (raw $ T.unwords [city, postalCode])
        (raw $ fromMaybe "" country)
    Region region -> comm1 "address" (raw region)
  foldMap (\t -> optFixComm "phone" 1 ["mobile", raw t]) (phone basics)
  document $ do
    comm2 "renewcommand" (commS "namefont") $ do
      comm2 "fontsize" 30 32
      commS "mdseries"
      commS "upshape"
    comm0 "makecvtitle"
    mapM_ mkSection sections

pandocHeader :: LaTeXReader ()
pandocHeader = do
  comm2 "providecommand" (comm0 "tightlist") $ do
    comm2 "setlength" (commS "itemsep") "0pt"
    comm2 "setlength" (commS "parskip") "0pt"

mkSocial :: Text -> Social -> LaTeXReader ()
mkSocial service Social {..} = optFixComm "social" 1 [raw service, raw user]

mkSection :: Section Text -> LaTeXReader ()
mkSection Section {..} = do
  section (raw heading)
  case content of
    Paragraph t -> raw t
    Work xs -> mapM_ mkJob xs
    Volunteering xs -> mapM_ mkVolunteer xs
    Education xs -> mapM_ mkStudy xs
    Skills xs -> mapM_ mkSkill xs
    _ -> error "not implemented"
mkSection BibTeXPublications {..} = lift (asks bibFile) >>= \case
  Just bibFile -> do
    comm2 "renewcommand" (commS "refname") (raw pubsHeading)
    mapM_ (comm1 "nocite" . raw) citeKeys
    comm1 "bibliographystyle" "plain"
    comm1 "bibliography" (fromString bibFile)
  Nothing -> mempty

mkJob :: Job Text -> LaTeXReader ()
mkJob Job {..} =
  comm6
    "cventry"
    (mkDateRange jobStartDate jobEndDate)
    (raw position)
    (raw company)
    (foldMap raw jobLocation)
    ""
    (foldMap raw jobSummary)

mkVolunteer :: Volunteer Text -> LaTeXReader ()
mkVolunteer Volunteer {..} =
  comm6
    "cventry"
    (mkDateRange volunteerStartDate volunteerEndDate)
    (raw volunteerPosition)
    (raw organization)
    (foldMap raw volunteerLocation)
    ""
    (foldMap raw volunteerSummary)

mkStudy :: Study Text -> LaTeXReader ()
mkStudy Study {..} =
  comm6
    "cventry"
    (mkDateRange studyStartDate studyEndDate)
    (raw (studyType <> ", " <> area))
    (raw institution)
    (foldMap raw studyLocation)
    ""
    (foldMap raw studySummary)

mkDateRange :: LaTeXC l => Date -> Maybe Date -> l
mkDateRange startDate =
  maybe (mkDate startDate) (\d -> mkDate startDate <> "--" <> mkDate d)

mkDate :: LaTeXC l => Date -> l
mkDate Date {..} = fromString $ printf "%02d/%02d" month (year `mod` 100)

mkSkill :: Skill Text -> LaTeXReader ()
mkSkill Skill {..} = case skillSummary of
  Just summary -> comm2 "cvitem" (raw skillArea) (raw summary)
  Nothing ->
    error
      "Rendering skill keyword lists is not yet implemented. Please use 'skillSummary' instead."
