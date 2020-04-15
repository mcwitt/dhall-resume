  λ(a : Type)
→ { basics : ./Basics.dhall
  , profiles : ./Profiles.dhall
  , headline : Optional a
  , sections : List (./Section.dhall a)
  }
