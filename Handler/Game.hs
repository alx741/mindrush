{-# LANGUAGE DeriveGeneric #-}

module Handler.Game where

import Import
import Data.Aeson
import Answer


data GameState = GameState
    { playersCount :: Int
    , winner :: Maybe Player
    , self :: Maybe Player
    , players :: [Player]
    }
    deriving (Generic, Show)

instance ToJSON GameState where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON GameState

getGameState = do
    mselfName <- lookupSession "name"
    let selfName = fromMaybe "Anónimo" mselfName

    self <- runDB $ selectList [PlayerName ==. selfName] []
    players <- runDB $ selectList [PlayerName !=. selfName] [Desc PlayerName]
    let players' = fmap entityVal players
    let winner = filter (\p -> playerProgress p >= 10) players'

    return $ GameState
        (length players + 1)
        (headMay winner)
        (headMay (fmap entityVal self))
        players'


getGameStateR :: Handler Value
getGameStateR = do
    gameState <- getGameState
    return $ toJSON gameState


getGameR :: Handler Html
getGameR = do
    mname <- lookupGetParam "name"

    sessionName <- lookupSession "name"
    case sessionName of
        Just name -> defaultLayout $ setTitle "Speed Ask" >> $(widgetFile "gameBoard")
        Nothing -> case mname of
                Just name -> newPlayer name
                Nothing -> redirect HomeR
    where
        newPlayer name = do
            addPlayer name
            setSession "name" name
            gameState <- getGameState
            liftIO $ print gameState
            defaultLayout $ setTitle "Speed Ask" >> $(widgetFile "gameBoard")

        addPlayer name = do
            players <- runDB $ selectList [] [Desc PlayerName]
            if length players >= 3
                then do
                    setMessage "El limite es de 3 jugadores"
                    redirect HomeR
                else runDB $ insert $ Player name 0


getPurgeR :: Handler Text
getPurgeR = do
    runDB $ deleteWhere ([] :: [Filter Player])
    return "Purged"

getPopulateR :: Handler Text
getPopulateR = do
    runDB $ deleteWhere ([] :: [Filter Question])
    runDB $ forM questions insert
    return "Populated"

questions :: [Question]
questions =
    [ Question
        "wut 1?"
        (Answer 1 "a1")
        (Answer 2 "a2")
        (Answer 3 "a3")
        (Answer 4 "a4")
        2
    , Question
        "wut 2?"
        (Answer 1 "a1")
        (Answer 2 "a2")
        (Answer 3 "a3")
        (Answer 4 "a4")
        2
    , Question
        "wut 3?"
        (Answer 1 "a1")
        (Answer 2 "a2")
        (Answer 3 "a3")
        (Answer 4 "a4")
        2
    , Question
        "wut 4?"
        (Answer 1 "a1")
        (Answer 2 "a2")
        (Answer 3 "a3")
        (Answer 4 "a4")
        2
    ]

postGameR :: Handler Html
postGameR = error "Not yet implemented: postGameR"
