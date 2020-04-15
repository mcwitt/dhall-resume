  λ(a : Type)
→ { position : Text
  , company : Text
  , jobStartDate : ./Date.dhall
  , jobEndDate : Optional ./Date.dhall
  , jobLocation : Optional Text
  , companyUrl : Optional Text
  , jobSummary : Optional a
  }
