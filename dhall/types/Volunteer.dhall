  λ(a : Type)
→ { volunteerPosition : Text
  , organization : Text
  , volunteerStartDate : ./Date.dhall
  , volunteerEndDate : Optional ./Date.dhall
  , volunteerLocation : Optional Text
  , organizationUrl : Optional Text
  , volunteerSummary : Optional a
  }
