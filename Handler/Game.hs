{-# LANGUAGE DeriveGeneric #-}

module Handler.Game where

import Import
import Prelude (read)
import System.Random
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
        Just name ->
            defaultLayout $ setTitle "Speed Ask" >> $(widgetFile "gameBoard")
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

getQuestionR :: Handler Value
getQuestionR = do
    key <- liftIO $ randomRIO (1, length questions)
    mquestion <- runDB $ selectFirst [QuestionKey ==. key] []
    case mquestion of
        Just question -> return $ toJSON $ entityVal question
        Nothing -> notFound

postAnswerR :: Handler Value
postAnswerR = do
    postData <- fmap fst runRequestBody
    mselfName <- lookupSession "name"
    let selfName = fromMaybe "Anónimo" mselfName

    let
        questionKey :: Maybe Int
        questionKey = fmap (read . unpack . snd) $ headMay postData

        answerId :: Maybe Int
        answerId = fmap (read . unpack . snd) $ lastMay postData

    Just (Entity _ question) <- case questionKey of
            Just qKey -> runDB $ selectFirst [QuestionKey ==. qKey] []
            Nothing -> notFound

    let correctness = case answerId of
            Just ansId -> questionCorrectAnswer question == ansId
            Nothing -> False

    if correctness
        then do
            increaseProgress selfName
            return $ toJSON ("correct" :: Text)
        else do
            decreaseProgress selfName
            return $ toJSON ("wrong" :: Text)

    where
        increaseProgress name = do
            mplayer <- runDB $ selectFirst [PlayerName ==. name] []
            case mplayer of
                Just (Entity playerId player) ->
                    if playerProgress player == 10
                        then return ()
                        else runDB $ update playerId [PlayerProgress +=. 1]
                Nothing -> notFound

        decreaseProgress name = do
            mplayer <- runDB $ selectFirst [PlayerName ==. name] []
            case mplayer of
                Just (Entity playerId player) ->
                    if playerProgress player == 0
                        then return ()
                        else runDB $ update playerId [PlayerProgress -=. 1]
                Nothing -> notFound


questions :: [Question]
questions =
    [ Question
        1
        "wut 1?"
        (Answer 1 "wut1 a1")
        (Answer 2 "wut1 a2")
        (Answer 3 "wut1 a3")
        (Answer 4 "wut1 a4")
        2
    , Question
        2
        "wut 2?"
        (Answer 1 "wut2 a1")
        (Answer 2 "wut2 a2")
        (Answer 3 "wut2 a3")
        (Answer 4 "wut2 a4")
        2
    , Question
        3
        "wut 3?"
        (Answer 1 "wut3 a1")
        (Answer 2 "wut3 a2")
        (Answer 3 "wut3 a3")
        (Answer 4 "wut3 a4")
        2
    , Question
        4
        "wut 4?"
        (Answer 1 "wut4 a1")
        (Answer 2 "wut4 a2")
        (Answer 3 "wut4 a3")
        (Answer 4 "wut4 a4")
        2
    ]
