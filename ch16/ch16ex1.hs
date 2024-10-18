data Creator = AuthorCreator Author | ArtistCreator Artist

data Author = Author Name

data Artist = Person Name | Band String

data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName
          | TowInitialWithLast Char Char LastName

type FirstName = String
type MiddleName = String
type LastName = String

data Book = Book {
    author    :: Creator
  , isbn      :: String
  , bookTitle :: String
  , bookYear  :: Int
  , bookPrice :: Double
}

data VenylRecord = VenylRecord {
    artist      :: Creator
  , recordTitle :: String
  , recordYear  :: Int
  , recordPrice :: Double
}

data CollectableToy = CollectableToy {
    name        :: String
  , description :: String
  , toyPrice    :: Double
}

data StoreItem = BookItem Book
               | RecordItem VenylRecord
               | ToyItem CollectableToy
               | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem _) = 0.0

data Pamphlet = Pamphlet {
    pamphletTitle       :: String
  , pamphletDescription :: String
  , contactInfo         :: String
}
