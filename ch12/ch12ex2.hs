type FirstName = String
type LastName = String
type Age = Int
type Height = Int

data Sex = Male | Female deriving (Show, Eq)

data RhType = Pos | Neg deriving (Show, Eq)
data ABOType = A | B | AB | O deriving (Show, Eq)
data BloodType = BloodType ABOType RhType deriving Show

type MiddleName = String
data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName deriving Show

data Patient = Patient { name :: Name
                       , sex :: Sex
                       , age :: Int
                       , height :: Int
                       , weight :: Int
                       , bloodType :: BloodType }

patientSummary :: Patient -> String
patientSummary p =  "************\n"
                 ++ "Patient Name: " ++ lastName p ++ ", " ++ firstName p ++ "\n"
                 ++ "Sex: " ++ (show . sex) p ++ "\n"
                 ++ "Age: " ++ (show . age) p ++ "\n"
                 ++ "Height: " ++ (show . height) p ++ " in.\n"
                 ++ "Weight: " ++ (show . weight) p ++ " lbs.\n"
                 ++ "Blood Type: " ++ showBloodType p ++ "\n"

firstName :: Patient -> String
firstName p = case name p of
                  Name f _ -> f

lastName :: Patient -> String
lastName p = case name p of
                 Name _ l -> l
                 NameWithMiddle _ _ l -> l

showBloodType :: Patient -> String
showBloodType p = show abo ++ if rh == Pos then "+" else "-"
                  where BloodType abo rh = bloodType p

johnSmith :: Patient
johnSmith = Patient { name = Name "John" "Smith"
                    , sex = Male
                    , age = 46
                    , height = 72
                    , weight = 210
                    , bloodType = BloodType AB Pos }

main :: IO ()
main = putStr $ patientSummary johnSmith
