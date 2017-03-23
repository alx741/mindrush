module Handler.Game where

import Import

getGameR :: Handler Html
getGameR = defaultLayout $ do
    $(widgetFile "gameBoard")

postGameR :: Handler Html
postGameR = error "Not yet implemented: postGameR"
