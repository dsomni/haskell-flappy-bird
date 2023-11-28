module Model where

-- import Miso

-- type PlayerId = String

-- data Record = Record
--     { ident :: PlayerId
--     , score :: Int
--     } deriving (Eq, Show)

-- instance FromJSON Record where
--     parseJSON = withObject "Player" $ \v -> Player
--         <$> v .: "id"
--         <*> v .: "score"

-- instance ToJSON Record where
--     toJSON (Player ident score) = object
--         [ "id"    .= ident
--         , "score"  .= score
--         ]