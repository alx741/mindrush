{-# LANGUAGE DeriveGeneric #-}

module Handler.Game where

import Import
import Data.Aeson


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
    let selfName = case mselfName of
            Just name -> name
            Nothing -> "Anónimo"

    self <- runDB $ selectList [PlayerName ==. selfName] []
    players <- runDB $ selectList [PlayerName !=. selfName] [Desc PlayerName]
    let players' = fmap entityVal players
    let winner = filter (\p -> (playerProgress p) >= 10) players'

    return $ GameState
        ((length players) + 1)
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
        Just name -> do
            defaultLayout $ do
                $(widgetFile "gameBoard")
        Nothing -> do
            let name = case mname of
                    Just n -> n
                    Nothing -> "Anónimo"
            newPlayer name
    where
        newPlayer name = do
            addPlayer name
            setSession "name" name
            gameState <- getGameState
            liftIO $ print gameState
            defaultLayout $ do
                $(widgetFile "gameBoard")

addPlayer name = do
    players <- runDB $ selectList [] [Desc PlayerName]
    if length players >= 3
        then do
            setMessage "El limite es de 3 jugadores"
            redirect HomeR
        else runDB $ insert $ Player name 0

getPurgeR :: Handler ()
getPurgeR = do
    runDB $ deleteWhere ([] :: [Filter Player])
    return ()

postGameR :: Handler Html
postGameR = error "Not yet implemented: postGameR"
