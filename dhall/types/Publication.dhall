  λ(a : Type)
→ { publicationTitle : Text
  , publisher : Text
  , publicationDate : ./Date.dhall
  , publicationUrl : Optional Text
  , publicationSummary : Optional a
  }
