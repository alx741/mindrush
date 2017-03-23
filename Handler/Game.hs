module Handler.Game where

import Import

getGameR :: Handler Html
getGameR = do
    mname <- lookupGetParam "name"
    let name = case mname of
            Just name -> name
            Nothing -> "Anónimo"

    addPlayer name

    defaultLayout $ do
        $(widgetFile "gameBoard")

addPlayer name = do
    players <- runDB $ selectList [] [Desc PlayerName]
    if length players >= 3
        then do
            setMessage "El limite es de 3 jugadores"
            redirect HomeR
        else runDB $ insert $ Player name

postGameR :: Handler Html
postGameR = error "Not yet implemented: postGameR"
