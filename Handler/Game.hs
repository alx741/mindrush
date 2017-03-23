module Handler.Game where

import Import

getGameR :: Handler Html
getGameR = do
    mname <- lookupGetParam "name"
    let name = case mname of
            Just name -> name
            Nothing -> "Anónimo"

    defaultLayout $ do
        $(widgetFile "gameBoard")

postGameR :: Handler Html
postGameR = error "Not yet implemented: postGameR"
