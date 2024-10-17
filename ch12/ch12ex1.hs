type FirstName = String
type LastName = String
type Age = Int
type Height = Int

data Sex = Male | Female deriving Show

data RhType = Pos | Neg deriving Show
data ABOType = A | B | AB | O deriving Show
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

canDonateTo :: Patient -> Patient -> Bool
canDonateTo p1 p2 = case (bloodType p1, bloodType p2) of
                         (BloodType O _, _) -> True
                         (_, BloodType AB _) -> True
                         (BloodType A _, BloodType A _) -> True
                         (BloodType B _, BloodType B _) -> True
                         otherwise -> False
