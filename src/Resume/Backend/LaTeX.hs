{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Resume.Backend.LaTeX
  ( renderText,
  )
where

import Control.Monad ((>=>))
import Data.Maybe
import qualified Data.Text as T
import Resume.Types as R
import Text.LaTeX
import Text.LaTeX.Base.Syntax
import Text.Pandoc

-- | Render Resume with Markdown-formatted text as LaTeX
renderText :: Resume Markdown -> Either PandocError Text
renderText r = render . toLaTeX <$> traverse fromMarkdown r
  where
    fromMarkdown = runPure . (readMarkdown def >=> writeLaTeX def) . unMarkdown

toLaTeX :: Resume Text -> LaTeX
toLaTeX Resume {..} =
  documentclass [FontSize (Pt 11), Paper A4] "moderncv"
    <> pandocHeader
    <> usepackage [TeXRaw "scale=0.8"] "geometry"
    <> TeXComm "moderncvstyle" [FixArg "casual"]
    <> TeXComm "moderncvcolor" [FixArg "blue"]
    <> maybe mempty (title . TeXRaw) headline
    <> TeXComm
      "name"
      (fixArgs [firstName $ name basics, lastName $ name basics])
    <> TeXComm "email" [FixArg . TeXRaw $ email basics]
    <> maybe
      mempty
      (\R.Link {url = url} -> TeXComm "homepage" [FixArg $ TeXRaw url])
      (homepage profiles)
    <> maybe mempty (mkSocial "linkedin") (linkedin profiles)
    <> maybe mempty (mkSocial "twitter") (twitter profiles)
    <> maybe mempty (mkSocial "github") (github profiles)
    <> case location basics of
      StreetAddress {..} ->
        TeXComm
          "address"
          ( fixArgs
              [address, T.unwords [city, postalCode], fromMaybe "" country]
          )
      _ -> mempty
    <> maybe
      mempty
      (\p -> TeXComm "phone" [OptArg "mobile", FixArg $ TeXRaw p])
      (phone basics)
    <> document (TeXCommS "makecvtitle" <> mconcat (fmap mkSection sections))

fixArgs :: [Text] -> [TeXArg]
fixArgs = fmap $ FixArg . TeXRaw

pandocHeader :: LaTeX
pandocHeader =
  TeXComm
    "providecommand"
    [ FixArg $ TeXCommS "tightlist",
      FixArg $ setlength "itemsep" "0pt" <> setlength "parskip" "0pt"
    ]
  where
    setlength k v = TeXComm "setlength" [FixArg $ TeXCommS k, FixArg $ TeXRaw v]

mkSocial :: Text -> Social -> LaTeX
mkSocial service Social {..} =
  TeXComm "social" [OptArg $ TeXRaw service, FixArg $ TeXRaw user]

mkSection :: Section Text -> LaTeX
mkSection Section {..} = section (TeXRaw heading) <> case content of
  Paragraph t -> TeXRaw t
  Work xs -> mconcat $ fmap mkJob xs
  Volunteering xs -> mconcat $ fmap mkVolunteer xs
  Education xs -> mconcat $ fmap mkStudy xs
  Skills xs -> mconcat $ fmap mkSkill xs
  Publications _ -> error "not implemented"
  Awards _ -> error "not implemented"
  Languages _ -> error "not implemented"
  Interests _ -> error "not implemented"

mkJob :: Job Text -> LaTeX
mkJob Job {..} =
  TeXComm "cventry" $
    fixArgs
      [ mkDateRange jobStartDate jobEndDate,
        position,
        company,
        fromMaybe "" jobLocation,
        "",
        fromMaybe "" jobSummary
      ]

mkVolunteer :: Volunteer Text -> LaTeX
mkVolunteer Volunteer {..} =
  TeXComm "cventry" $
    fixArgs
      [ mkDateRange volunteerStartDate volunteerEndDate,
        volunteerPosition,
        organization,
        fromMaybe "" volunteerLocation,
        "",
        fromMaybe "" volunteerSummary
      ]

mkStudy :: Study Text -> LaTeX
mkStudy Study {..} =
  TeXComm "cventry" $
    fixArgs
      [ mkDateRange studyStartDate studyEndDate,
        studyType <> ", " <> area,
        institution,
        fromMaybe "" studyLocation,
        fromMaybe "" gpa,
        fromMaybe "" studySummary
      ]

mkDateRange :: Date -> Maybe Date -> Text
mkDateRange startDate =
  maybe (mkDate startDate) (\m -> mkDate startDate <> "--" <> mkDate m)

mkDate :: Date -> Text
mkDate Date {..} = fromString $ show month ++ "/" ++ show year

mkSkill :: Skill Text -> LaTeX
mkSkill Skill {..} = case skillSummary of
  Just summary -> TeXComm "cvitem" $ fixArgs [skillArea, summary]
  Nothing ->
    error
      "Rendering skill keyword lists is not yet implemented. Please use 'skillSummary' instead."
