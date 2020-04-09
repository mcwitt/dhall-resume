  λ(a : Type)
→ { name : ./Name.dhall
  , headline : Optional Text
  , city : Text
  , contact : ./ContactInfo.dhall
  , sections : List (./CVSection.dhall a)
  }
