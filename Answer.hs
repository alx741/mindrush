{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Answer where

import Database.Persist.TH
import Data.Text
import Data.Aeson
import GHC.Generics (Generic)
import Prelude (Int, Show, Read, Eq)

data Answer = Answer
    { answerId :: Int
    , answerText :: Text
    }
    deriving (Generic, Show, Read, Eq)

instance ToJSON Answer where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Answer

derivePersistField "Answer"
