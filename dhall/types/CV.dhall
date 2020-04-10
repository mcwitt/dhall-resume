  λ(a : Type)
→ { basics : ./Basics.dhall
  , profiles : ./Profiles.dhall
  , headline : Optional a
  , sections : List (./CVSection.dhall a)
  }
