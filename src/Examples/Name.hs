module Examples.Name
where
data Cite = Citation (Maybe String) String | Example String | Footnote String deriving (Show)
data Name = Name { name :: String
                 , cite :: Cite  } deriving (Show)
