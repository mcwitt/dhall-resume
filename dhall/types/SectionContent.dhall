  λ(a : Type)
→ < Paragraph : a
  | Work : List (./Job.dhall a)
  | Volunteering : List (./Volunteer.dhall a)
  | Skills : List (./Skill.dhall a)
  | Education : List (./Study.dhall a)
  | Awards : List (./Award.dhall a)
  | Publications : List (./Publication.dhall a)
  | BibTeXPublications : List Text
  | Languages : List ./Language.dhall
  | Interests : List (./Interest.dhall a)
  >
