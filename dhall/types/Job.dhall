  λ(a : Type)
→ { position : Text
  , company : Text
  , jobStartDate : ./CVDate.dhall
  , jobEndDate : Optional ./CVDate.dhall
  , jobLocation : Optional Text
  , companyUrl : Optional Text
  , jobSummary : Optional a
  }
