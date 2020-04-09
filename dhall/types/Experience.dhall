  λ(a : Type)
→ { title : Text
  , organization : Text
  , location : Text
  , startMonth : ./CVDate.dhall
  , endMonth : Optional ./CVDate.dhall
  , items : List a
  }
