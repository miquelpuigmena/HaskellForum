
{-# LANGUAGE OverloadedStrings #-}

module View
where
import Model

import Data.Monoid
import Data.Text as T

-- ****************************************************************
-- Views

pageHtml :: [(TaskId, Task)] -> Text
pageHtml tasks =
    let html1 =    "<!DOCTYPE html>\n"
                <> "<html><head>\n"
                <> "  <title>Tasques</title>\n"
                <> "  <link rel='stylesheet' type='text/css' href='https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css'>\n"
                <> "</head><body><div class='container-fluid'>\n"
                <> "<h1>Tasques</h1>\n"
                <> "<form class='form-horizontal' role='form' method='POST' action='#'>\n"
                <> "  <div class='form-group'>\n"
                <> "    <label class='control-label col-sm-2' for='title'>Títol:</label>\n"
                <> "    <div class='col-sm-10'>\n"
                <> "      <input type='text' class='form-control' id='title' name='title' placeholder='Introduiu el títol de la nova tasca'>\n"
                <> "    </div>\n"
                <> "  </div>\n"
                <> "  <div class='form-group'>\n"
                <> "    <div class='col-sm-offset-2 col-sm-10'>\n"
                <> "      <button type='submit' class='btn btn-primary' name='add'>Afegeix</button>\n"
                <> "    </div>\n"
                <> "  </div>\n"
                <> "</form>\n"
                <> "<hr>\n"
                <> "<form method='POST' action='#'><table class='table table-striped table-condensed'>\n"
                <> "  <thead><tr><th>&nbsp;</th><th>Títol</th><th>Estat</th></tr></thead><tbody>\n"
        taskToHtml (tid, Task title done) =
                   "  <tr><td><input type='checkbox' name='tid' value='" <> T.pack (show tid) <> "'></td>\n"
                <> "      <td>" <> escapeHtml title <> "</td><td>" <> (if done then "FET" else "PENDENT") <> "</td></tr>\n"
        html2 =    "</tbody></table>\n"
                <> "<div class='form-group'>\n"
                <> "  <div class='col-sm-offset-2 col-sm-2'>\n"
                <> "    <button type='submit' class='btn btn-primary' name='mark'>Marca FET</button>"
                <> "  </div>\n"
                <> "  <div class='col-sm-2'>\n"
                <> "    <button type='submit' class='btn btn-primary' name='delete'>Elimina</button>\n"
                <> "  </div>\n"
                <> "</div>\n"
                <> "</form>\n"
                <> "</div></body></html>\n"
    in html1 <> foldMap taskToHtml tasks <> html2

escapeHtml :: Text -> Text
escapeHtml =
    let convert '<'  = "&lt;"
        convert '&'  = "&amp;"
        convert '>'  = "&gt;"
        convert '\"' = "&quot;"
        convert '\'' = "&apos;"
        convert c    = T.singleton c
    in T.concatMap convert

