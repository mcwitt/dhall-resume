  λ(a : Type)
→ { studyType : Text
  , area : Text
  , institution : Text
  , studyStartDate : ./Date.dhall
  , studyEndDate : Optional ./Date.dhall
  , studyLocation : Optional Text
  , institutionUrl : Optional Text
  , gpa : Optional Text
  , courses : List a
  , studySummary : Optional a
  }
