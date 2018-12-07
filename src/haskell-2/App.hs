
{-# LANGUAGE OverloadedStrings #-}

module App
where
import Model
import View

import Network.Wai
import Network.HTTP.Types
import qualified Network.Wai.Middleware.Approot as AR(getApproot)

import Data.Monoid
import Data.Maybe
import Data.List as L
import Data.Text as T
import Data.Text.Encoding as T
import Data.ByteString as B
import Text.Read
import Control.Monad

-- ****************************************************************
-- WAI application

makeApp :: IO Application
makeApp = do
    -- Open the database (the model state)
    db <- openDb "tasks.db"
    -- Return the application service
    pure (service db)

service :: TaskDb -> Application
service db req respond = do
    let meth = requestMethod req
    if meth == methodGet then
        getHome db req respond
    else if meth == methodPost then
        postHome db req respond
    else
        respond $ errorResponse methodNotAllowed405 $ "Invalid request method " <> T.pack (show meth)

-- ****************************************************************
-- Controller handlers

getHome :: TaskDb -> Application
getHome db req respond = do
    -- Get model info
    tasks <- getTaskList db
    -- Get HTML content
    let html = pageHtml tasks
    -- Generate response
    respond $ htmlResponse html

postHome :: TaskDb -> Application
postHome db req respond = do
    let approot = requestApproot req
    q <- getPostQuery req
    if isJust (lookupParam "add" q) then
        case lookupParam "title" q of
            Just ""    -> respond $ invalidArg "title"
            Just title -> do
                addTask title db
                respond $ redirectResponse (approot <> homePath)
            Nothing    -> respond $ invalidArg "title"
    else if isJust (lookupParam "mark" q) then
        case mapM fromText (lookupParams "tid" q) of
            Just tids -> do
                forM tids $ \ tid ->
                    markTask tid db
                respond $ redirectResponse (approot <> homePath)
            Nothing -> respond $ invalidArg "tid"
    else if isJust (lookupParam "delete" q) then
        case mapM fromText (lookupParams "tid" q) of
            Just tids -> do
                forM tids $ \ tid ->
                    deleteTask tid db
                respond $ redirectResponse (approot <> homePath)
            Nothing -> respond $ invalidArg "tid"
    else
        respond $ errorResponse badRequest400 "Invalid POST action"
  where
    fromText :: Read a => Text -> Maybe a
    fromText = readMaybe . T.unpack
    invalidArg arg = errorResponse badRequest400 ("Invalid arguments: " <> arg)

homePath :: Text
homePath = "/"

-- ****************************************************************
-- Controller utilities

mimeHtml :: ByteString
mimeHtml = "text/html;charset=UTF-8"

-- Obté la URL del CGI que està executant
requestApproot :: Request -> Text
requestApproot req =
    T.decodeUtf8 (AR.getApproot req)

getPostQuery :: Request -> IO Query
getPostQuery req =
    parseQuery <$> getBody req
    where
        getBody req = do
            b <- requestBody req
            if B.null b then pure B.empty
            else do
                bs <- getBody req
                pure $ b <> bs

lookupParam :: Text -> Query -> Maybe Text
lookupParam name query =
    case lookupParams name query of
        []    -> Nothing
        (x:_) -> Just x

lookupParams :: Text -> Query -> [Text]
lookupParams name query =
    let nameBS = T.encodeUtf8 name
    in T.decodeUtf8 <$> catMaybes (snd <$> L.filter ((==) nameBS . fst) query)

redirectResponse :: Text -> Response
redirectResponse url =
    let headers = [ ("Location", T.encodeUtf8 url)
                  , ("Content-Type", "text/plain;charset=UTF-8") ]
    in responseBuilder seeOther303 headers (T.encodeUtf8Builder "Redirect")

htmlResponse :: Text -> Response
htmlResponse html =
    let headers = [ ("Content-Type", mimeHtml) ]
    in responseBuilder ok200 headers (T.encodeUtf8Builder html)

errorResponse :: Status -> Text -> Response
errorResponse status msg =
    let headers = [ ("Content-Type", mimeHtml) ]
        html = "<!DOCTYPE html><html><head><title>Exemple CGI Lib: Tasques</title></head><body>\n"
                <> "<center><h2>ERROR</h2><h3><font color=\"red\">" <> msg <> "</font></h3></center>\n"
                <> "</body></html>\n"
    in responseBuilder status headers (T.encodeUtf8Builder html)

