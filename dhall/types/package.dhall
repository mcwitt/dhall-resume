{ mkTypes =
      λ(a : Type)
    → { CV = ./CV.dhall a
      , CVDate = ./CVDate.dhall
      , CVSection = ./CVSection.dhall a
      , ContactInfo = ./ContactInfo.dhall
      , Experience = ./Experience.dhall a
      , SectionContent = ./SectionContent.dhall a
      , SkillCategory = ./SkillCategory.dhall
      }
}
