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
    let selfName = fromMaybe "Anonymous" mselfName

    self <- runDB $ selectList [PlayerName ==. selfName] []
    otherPlayers <- runDB $
        selectList [PlayerName !=. selfName] [Desc PlayerName]
    let otherPlayers' = fmap entityVal otherPlayers
    winner <- runDB $ selectFirst [PlayerProgress >=. 10] []

    return $ GameState
        (length otherPlayers + 1)
        (fmap entityVal winner)
        (headMay (fmap entityVal self))
        otherPlayers'


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
            defaultLayout $ setTitle "Mind Rush" >> $(widgetFile "gameBoard")
        Nothing -> case mname of
                Just name -> newPlayer name
                Nothing -> redirect HomeR
    where
        newPlayer name = do
            addPlayer name
            setSession "name" name
            gameState <- getGameState
            liftIO $ print gameState
            defaultLayout $ setTitle "Mind Rush" >> $(widgetFile "gameBoard")

        addPlayer name = do
            players <- runDB $ selectList [] [Desc PlayerName]
            if length players >= 3
                then do
                    setMessage "There is a match going on, please try againt later"
                    redirect HomeR
                else runDB $ insert $ Player name 0


getPurgeR :: Handler Html
getPurgeR = do
    runDB $ deleteWhere ([] :: [Filter Player])
    redirect HomeR

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
    let selfName = fromMaybe "Anonymous" mselfName

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
        "¿Cuál es la capital de Ecuador?"
        (Answer 1 "Cuenca")
        (Answer 2 "Guayaquil")
        (Answer 3 "Ambato")
        (Answer 4 "Quito")
        4

    , Question
        2
        "No tengo trono ni reina, ni nadie que me comprenda, pero sigo siendo el…"
        (Answer 1 "Rey")
        (Answer 2 "Papá de los helados")
        (Answer 3 "Papirruqui")
        (Answer 4 "Alma de la fiesta")
        1

    , Question
        3
        "¿Cuántas especies de cada animal llevó Moisés según la biblia en el arca antes del diluvio?"
        (Answer 1 "2 de cada especie.")
        (Answer 2 "Una pareja de cada especie")
        (Answer 3 "Ninguno")
        (Answer 4 "Todos menos los acuáticos")
        3

    , Question
        4
        "¿De qué país es el futbolista Cristiano Ronaldo?"
        (Answer 1 "España")
        (Answer 2 "Portugal")
        (Answer 3 "Brasil")
        (Answer 4 "Polonia")
        2

    , Question
        5
        "¿Cuál es el idioma oficial de Israel?"
        (Answer 1 "Hebreo")
        (Answer 2 "Griego")
        (Answer 3 "Catalán")
        (Answer 4 "Hindi")
        1

    , Question
        6
        "¿Cual de estos animales es un rumiante?"
        (Answer 1 "La ardilla")
        (Answer 2 "El caracol")
        (Answer 3 "El loro")
        (Answer 4 "La vaca")
        4

    , Question
        7
        "¿Cuál es el gentilicio de una persona nacida en Bélgica?"
        (Answer 1 "Belgatario")
        (Answer 2 "Belga")
        (Answer 3 "Belganés")
        (Answer 4 "Belganita")
        2

    , Question
        8
        "Una persona que tiene dos o más parejas está practicando:"
        (Answer 1 "El polimorfismo")
        (Answer 2 "El politeísmo")
        (Answer 3 "La poligamia")
        (Answer 4 "La poligrafía")
        3

    , Question
        9
        "¿Cuál de estas chicas vive en el bosque con 7 enanitos?"
        (Answer 1 "Rapunzel")
        (Answer 2 "Blancanieves")
        (Answer 3 "La Bella Durmiente")
        (Answer 4 "La Cenicienta")
        2

    , Question
        10
        "Cuando un grupo de personas marchan una detrás de otra, se dice que marchan en fila:"
        (Answer 1 "Griega")
        (Answer 2 "Rusa")
        (Answer 3 "India")
        (Answer 4 "China")
        3

    , Question
        11
        "Que lee un quiromante para predecir el futuro:"
        (Answer 1 "Las cartas")
        (Answer 2 "Las estrellas")
        (Answer 3 "Las líneas de la mano")
        (Answer 4 "Las velas")
        3

    , Question
        12
        "¿Cuál de los siguientes, NO es un elemento químico?"
        (Answer 1 "Cobre")
        (Answer 2 "Calcio")
        (Answer 3 "Bronce")
        (Answer 4 "Fósforo")
        3

    , Question
        13
        "El albinismo se presenta por la carencia de:"
        (Answer 1 "Pigmentación")
        (Answer 2 "Calcio")
        (Answer 3 "Oxígeno")
        (Answer 4 "Vitamina A")
        1

    , Question
        14
        "Uno de los libros de J. K. Rowling es Harry Potter y:"
        (Answer 1 "El arca perdida")
        (Answer 2 "El misterio del príncipe")
        (Answer 3 "La magia negra")
        (Answer 4 "La orden secreta de la magia")
        2

    , Question
        15
        "Las Vegas es una ciudad estadounidense que queda en:"
        (Answer 1 "Oklahoma")
        (Answer 2 "Nevada")
        (Answer 3 "Arizona")
        (Answer 4 "Colorado")
        2

    , Question
        16
        "¿Que clase de palabra es \"según\"?"
        (Answer 1 "Verbo")
        (Answer 2 "Preposición")
        (Answer 3 "Adjetivo")
        (Answer 4 "Sustantivo")
        2

    , Question
        17
        "¿Cuál de los siguientes países no ha adoptado el euro?"
        (Answer 1 "Luxemburgo")
        (Answer 2 "Grecia")
        (Answer 3 "Dinamarca")
        (Answer 4 "Finlandia")
        3

    , Question
        18
        "Fecha de Independencia del Ecuador"
        (Answer 1 "10 de agosto de 1810")
        (Answer 2 "24 de mayo de 1810")
        (Answer 3 "10 de agosto de 1820")
        (Answer 4 "24 de mayo de 1820")
        1

    , Question
        19
        "¿Cuál de estas obras no fue escrita por William Shakespeare?"
        (Answer 1 "Romeo y Julieta")
        (Answer 2 "Hamlet")
        (Answer 3 "La Divina Comedia")
        (Answer 4 "Las alegres comadres de Windsor")
        3

    , Question
        20
        "La palabra \"trapo\" proviene del:"
        (Answer 1 "Griego")
        (Answer 2 "Árabe")
        (Answer 3 "Latín")
        (Answer 4 "Chino")
        3

    , Question
        21
        "¿Cuál de los siguientes países limita con Francia?"
        (Answer 1 "Croacia")
        (Answer 2 "Alemania")
        (Answer 3 "Portugal")
        (Answer 4 "Turquía")
        2

    , Question
        22
        "En la mitología griega, Eros es considerado el dios del:"
        (Answer 1 "Guerra")
        (Answer 2 "Tierra")
        (Answer 3 "Amor")
        (Answer 4 "Mar")
        3

    , Question
        23
        "La jibia es un:"
        (Answer 1 "Insecto")
        (Answer 2 "Reptil")
        (Answer 3 "Molusco")
        (Answer 4 "Pez")
        3

    , Question
        24
        "La coroides es una membrana que se encuentra en:"
        (Answer 1 "El corazón")
        (Answer 2 "El ojo")
        (Answer 3 "El pulmón")
        (Answer 4 "El cerebro")
        2

    , Question
        25
        "¿Cuál es el significado del prefijo \"epi-\"?"
        (Answer 1 "Alrededor")
        (Answer 2 "Sobre")
        (Answer 3 "Inferior")
        (Answer 4 "Adentro")
        2

    , Question
        26
        "Es el espacio de tiempo durante el cual un Estado no tiene soberano:"
        (Answer 1 "Internodio")
        (Answer 2 "Internuncio")
        (Answer 3 "Intermezzo")
        (Answer 4 "Interregno")
        4

    , Question
        27
        "Hitler murió debido a:"
        (Answer 1 "Suicidio")
        (Answer 2 "Homicidio")
        (Answer 3 "Accidente")
        (Answer 4 "Caído en guerra")
        1

    , Question
        28
        "Las pantallas táctiles modernas son:"
        (Answer 1 "Inductivas")
        (Answer 2 "Resistivas")
        (Answer 3 "Capacitivas")
        (Answer 4 "Transitivas")
        3

    , Question
        29
        "¿De dónde se extrae el alcohol metílico?"
        (Answer 1 "Madera")
        (Answer 2 "Animales")
        (Answer 3 "Caña")
        (Answer 4 "Basura")
        1

    , Question
        30
        "La segunda guerra mundial terminó en:"
        (Answer 1 "1930")
        (Answer 2 "1892")
        (Answer 3 "1990")
        (Answer 4 "1945")
        4

    , Question
        31
        "El nombre químico de la sal es:"
        (Answer 1 "Bicarbonato de Sodio")
        (Answer 2 "Cloruro de Sodio")
        (Answer 3 "Bicloruro de Sodio")
        (Answer 4 "Carbonato de Sodio")
        2
    ]
