  λ(a : Type)
→ { volunteerPosition : Text
  , organization : Text
  , volunteerStartDate : ./CVDate.dhall
  , volunteerEndDate : Optional ./CVDate.dhall
  , volunteerLocation : Optional Text
  , organizationUrl : Optional Text
  , volunteerSummary : Optional a
  }
