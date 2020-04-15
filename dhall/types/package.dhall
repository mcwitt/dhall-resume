{ mkTypes =
      λ(a : Type)
    → { Award = ./Award.dhall a
      , Basics = ./Basics.dhall
      , Resume = ./Resume.dhall a
      , Date = ./Date.dhall
      , Section = ./Section.dhall a
      , Interest = ./Interest.dhall a
      , Job = ./Job.dhall a
      , Language = ./Language.dhall
      , Location = ./Location.dhall
      , Name = ./Name.dhall
      , Profiles = ./Profiles.dhall
      , Publication = ./Publication.dhall a
      , SectionContent = ./SectionContent.dhall a
      , Skill = ./Skill.dhall a
      , Social = ./Social.dhall
      , Study = ./Study.dhall a
      , Volunteer = ./Volunteer.dhall a
      }
}
