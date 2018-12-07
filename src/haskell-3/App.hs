
{-# LANGUAGE OverloadedStrings #-}

module App
where
import Model
import View

import Network.Wai
import Network.HTTP.Types
import qualified Network.Wai.Middleware.Approot as AR(getApproot)
import Web.Cookie

import Data.Monoid
import Data.Maybe
import Data.List as L
import Data.Text as T
import Data.Text.Encoding as T
import Data.ByteString as B
import Data.ByteString.Builder
import Data.ByteString.Lazy as BL
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
        path = pathInfo req
    case path of
        [] ->
            if meth == methodGet then
                getLogin req respond
            else if meth == methodPost then
                postLogin db req respond
            else
                respond $ errorResponse methodNotAllowed405 $ "Invalid request method " <> T.pack (show meth)
        ["login"] ->
            if meth == methodGet then
                getLogin req respond
            else if meth == methodPost then
                postLogin db req respond
            else
                respond $ errorResponse methodNotAllowed405 $ "Invalid request method " <> T.pack (show meth)
        ["logout"] ->
            handleLogout req respond
        ["signup"] ->
            if meth == methodGet then
                getSignup req respond
            else if meth == methodPost then
                postSignup db req respond
            else
                respond $ errorResponse methodNotAllowed405 $ "Invalid request method " <> T.pack (show meth)
        _ -> respond $ errorResponse notFound404 $ "Invalid path " <> T.pack (show path)

-- ****************************************************************
-- Controller handlers

getLogin :: Application
getLogin req respond = do
    let approot = requestApproot req
        -- Get HTML content
        html = loginHtml approot
    -- Generate response
    respond $ htmlResponse html Nothing approot

postLogin :: TaskDb -> Application
postLogin db req respond = do
    let approot = requestApproot req
    q <- getPostQuery req
    -- Get user and pass from login form
    if isJust (lookupParam "user-login" q) then
        case lookupParam "user-login" q of
            Just ""    -> respond $ invalidArg "user"
            Nothing    -> respond $ invalidArg "user"
            Just user  -> do
                case lookupParam "password-login" q of
                    Just ""        -> respond $ invalidArg "password"
                    Nothing        -> respond $ invalidArg "password"
                    Just password  -> do
                        -- Get list of Users registered as Tuple (Username, Password) 
                        let userList = getUserList db
                        if isJust (lookup user userList) then
                                if fromJust (lookup user userList) == password then 
                                        respond $ redirectResponse (approot <> homePath) (Just user) approot
                                else respond $ errorResponse notFound404 $ "Incorrect password"
                         else respond $ errorResponse notFound404 $ "User not found"
     else respond $ errorResponse notFound404 $ "Incorrect log in."
  where
    invalidArg arg = errorResponse badRequest400 ("Invalid arguments: " <> arg)

getSignup :: Application
getSignup req respond = do
    let approot = requestApproot req
        -- Get HTML content
        html = signupHtml approot
        -- Generate response
    respond $ htmlResponse html Nothing approot

postSignup :: TaskDb -> Application
postSignup db req respond = do
    let approot = requestApproot req
    -- Get user and pass from signin form
    q <- getPostQuery req
    if isJust (lookupParam "user-signup" q) then
        case lookupParam "user-signup" q of
            Just ""    -> respond $ invalidArg "user"
            Nothing    -> respond $ invalidArg "user"
            Just user  -> do
                case lookupParam "password-signup" q of
                    Just ""        -> respond $ invalidArg "password"
                    Nothing        -> respond $ invalidArg "password"
                    Just password  -> do 
                        --TODO: 
                        FALTARIA COMPROBAR QUE NO HI FOS JA A LA BASE DE DADES
                        addUser user password db
                        respond $ redirectResponse (approot <> loginPath) (getSessionUser req) approot
     else respond $ errorResponse notFound404 $ "Incorrect sign up."
  where
    invalidArg arg = errorResponse badRequest400 ("Invalid arguments: " <> arg)


handleLogout :: Application
handleLogout req respond = do
    let approot = requestApproot req
    respond $ redirectResponse (approot <> homePath) Nothing approot

-- ----------------------------------------------------------------

getHome :: TaskDb -> Application
getHome db req respond = do
    let approot = requestApproot req
    -- Get model info
    tasks <- getTaskList db
    let mbuser = getSessionUser req
    -- Get HTML content
    let html = pageHtml tasks mbuser
    -- Generate response
    respond $ htmlResponse html mbuser approot


postHome :: TaskDb -> Application
postHome db req respond = do
    let approot = requestApproot req
    q <- getPostQuery req
    
    case getSessionUser req of
        Nothing ->
            respond $ redirectResponse (approot <> loginPath) Nothing approot
        Just user -> do
            if isJust (lookupParam "add" q) then
                case lookupParam "title" q of
                    Just ""    -> respond $ invalidArg "title"
                    Just title -> do
                        addTask title db
                        respond $ redirectResponse (approot <> homePath) (getSessionUser req) approot
                    Nothing    -> respond $ invalidArg "title"
            else if isJust (lookupParam "mark" q) then
                case mapM fromText (lookupParams "tid" q) of
                    Just tids -> do
                        forM tids $ \ tid ->
                            markTask tid db
                        respond $ redirectResponse (approot <> homePath) (getSessionUser req) approot
                    Nothing -> respond $ invalidArg "tid"
            else if isJust (lookupParam "delete" q) then
                case mapM fromText (lookupParams "tid" q) of
                    Just tids -> do
                        forM tids $ \ tid ->
                            deleteTask tid db
                        respond $ redirectResponse (approot <> homePath) (getSessionUser req) approot
                    Nothing -> respond $ invalidArg "tid"
                else
                        respond $ errorResponse badRequest400 "Invalid POST action"
             where
                fromText :: Read a => Text -> Maybe a
                fromText = readMaybe . T.unpack
                invalidArg arg = errorResponse badRequest400 ("Invalid arguments: " <> arg)
           

-- ****************************************************************
-- Controller utilities

mimeHtml :: B.ByteString
mimeHtml = "text/html;charset=UTF-8"

-- Obté la URL del CGI que està executant
requestApproot :: Request -> Text
requestApproot req =
    T.decodeUtf8 (AR.getApproot req)

getSessionUser :: Request -> Maybe Text
getSessionUser req = do
    cookieHeader <- lookup "Cookie" (requestHeaders req)
    euser <- lookup "user" (parseCookies cookieHeader)
    pure (decodeLatin1 euser)

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

redirectResponse :: Text -> Maybe Text -> Text -> Response
redirectResponse url mbuser appRoot =
    let headers = [ ("Location", T.encodeUtf8 url)
                  , ("Content-Type", "text/plain;charset=UTF-8")
                  , ("Set-Cookie", mkSetCookieValue mbuser appRoot) ]
    in responseBuilder seeOther303 headers (T.encodeUtf8Builder "Redirect")

htmlResponse :: Text -> Maybe Text -> Text -> Response
htmlResponse html mbuser appRoot =
    let headers = [ ("Content-Type", mimeHtml)
                  , ("Set-Cookie", mkSetCookieValue mbuser appRoot) ]
    in responseBuilder ok200 headers (T.encodeUtf8Builder html)

mkSetCookieValue :: Maybe Text -> Text -> B.ByteString
mkSetCookieValue mbuser appRoot =
    let setCookie = case mbuser of
           Nothing   -> defaultSetCookie { setCookieName = "user", setCookiePath = Just (T.encodeUtf8 appRoot)
                                         , setCookieValue = B.empty, setCookieMaxAge = Just 0 }
           Just user -> defaultSetCookie { setCookieName = "user", setCookiePath = Just (T.encodeUtf8 appRoot)
                                         , setCookieValue = T.encodeUtf8 user, setCookieMaxAge = Just 300 }
    in BL.toStrict $ toLazyByteString $ renderSetCookie setCookie

errorResponse :: Status -> Text -> Response
errorResponse status msg =
    let headers = [ ("Content-Type", mimeHtml) ]
        html = "<!DOCTYPE html><html><head><title>Exemple CGI Lib: Tasques</title></head><body>\n"
                <> "<center><h2>ERROR</h2><h3><font color=\"red\">" <> msg <> "</font></h3></center>\n"
                <> "</body></html>\n"
    in responseBuilder status headers (T.encodeUtf8Builder html)

authLoginUser :: Text -> IO [(Text, Text)] -> Maybe Text
authLoginUser user dbTuples =  (L.head [y | (x,y) <- dbTuples, x == user])


