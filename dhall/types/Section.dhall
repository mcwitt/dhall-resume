  λ(a : Type)
→ < Section : { heading : Text, content : ./SectionContent.dhall a }
  | BibTeXPublications : { pubsHeading : a, citeKeys : List Text }
  >
