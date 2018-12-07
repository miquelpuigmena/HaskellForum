
{-# LANGUAGE OverloadedStrings #-}

module App
where
import Model

import Network.Wai
import Network.HTTP.Types

import Data.Monoid
import Data.Text as T
import Data.Text.Encoding as T
import Data.ByteString as B

-- ****************************************************************
-- WAI application

makeApp :: IO Application
makeApp = do
    -- Open the database (the model state)
    db <- openDb "tasks.db"
    -- Return the WAI application
    pure (service db)

service :: TaskDb -> Application
service db req respond = do
    -- Get model info
    tasks <- getTaskList db
    -- Get HTML content
    let html = pageHtml tasks
    -- Generate response
    respond $ htmlResponse html

htmlResponse :: Text -> Response
htmlResponse html =
    let headers = [ ("Content-Type", mimeHtml) ]
    in responseBuilder ok200 headers (T.encodeUtf8Builder html)

mimeHtml :: ByteString
mimeHtml = "text/html;charset=UTF-8"

pageHtml :: [(TaskId, Task)] -> Text
pageHtml tasks =
    let taskToHtml (tid, Task title done) =
          "  <tr><td>" <> escapeHtml title <> "</td><td>" <> (if done then "FET" else "PENDENT") <> "</td></tr>\n"
    in    "<!DOCTYPE html>\n"
       <> "<html><head>\n"
       <> "  <title>Tasques</title>\n"
       <> "  <link rel='stylesheet' type='text/css' href='https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css'>\n"
       <> "</head><body><div class='container-fluid'>\n"
       <> "<h1>Tasques</h1>\n"
       <> "<table class='table table-striped table-condensed'>\n"
       <> "  <thead><tr><th>Títol</th><th>Estat</th></tr></thead><tbody>\n"
       <> foldMap taskToHtml tasks
       <> "  </tbody></table>\n"
       <> "</div></body></html>\n"

escapeHtml :: Text -> Text
escapeHtml =
    let convert '<'  = "&lt;"
        convert '&'  = "&amp;"
        convert '>'  = "&gt;"
        convert '\"' = "&quot;"
        convert '\'' = "&apos;"
        convert c    = T.singleton c
    in T.concatMap convert

