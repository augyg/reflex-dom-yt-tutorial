module Common.Api where

commonStuff :: String
commonStuff = "Here is a string defined in Common.Api"

data OnboardingForm = OnboardingForm Name Occupation Age

type Name = String
type Occupation = String
type Age = Int 
