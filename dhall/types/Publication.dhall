  λ(a : Type)
→ { publicationTitle : Text
  , publisher : Text
  , publicationDate : ./CVDate.dhall
  , publicationUrl : Optional Text
  , publicationSummary : Optional a
  }
