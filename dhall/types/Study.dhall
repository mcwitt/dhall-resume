  λ(a : Type)
→ { studyType : Text
  , area : Text
  , institution : Text
  , studyStartDate : ./CVDate.dhall
  , studyEndDate : Optional ./CVDate.dhall
  , studyLocation : Optional Text
  , institutionUrl : Optional Text
  , gpa : Optional Text
  , courses : List a
  , studySummary : Optional a
  }
