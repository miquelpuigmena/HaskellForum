
{-# LANGUAGE OverloadedStrings #-}

module View
where
import Model

import Data.Monoid
import Data.Text as T
import Data.Text.Encoding as T
import Data.ByteString
import Data.Maybe

homePath :: Text
homePath = "/"

loginPath :: Text
loginPath = "/login"

logoutPath :: Text
logoutPath = "/logout"

signupPath :: Text
signupPath = "/signup"

-- ****************************************************************
-- Views

-- NOTA: Per a construir URLs als diferents 'paths' de l'aplicacio podeu fer:
--      appRoot <> path
-- on appRoot es la URL del CGI i path es homePath, loginPath o logoutPath.

loginHtml :: Text -> Text
loginHtml appRoot =
    -- Mostra el formulari de login que permet introduir el nom i el password
    -- de l'usuari
        let html3 = "<!DOCTYPE html>\n"
                <> "<html lang='en' >\n"
                <> "<head>\n"
                <> "    <title>Sing in</title>\n"
                <> "    <meta charset='UTF-8' >\n"
                <> "<meta name='viewport' content='width=device-width, initial-scale=1'>\n"
                <> "<link rel='icon' type='image/png' href='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/images/icons/hs-icon.png'/>\n"
                <> "<link rel='stylesheet' type='text/css' href='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/vendor/bootstrap/css/bootstrap.min.css'>\n"
                <> "<link rel='stylesheet' type='text/css' href='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/fonts/font-awesome-4.7.0/css/font-awesome.min.css'>\n"
                <> "<link rel='stylesheet' type='text/css' href='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/vendor/animate/animate.css'>\n"
                <> "<link rel='stylesheet' type='text/css' href='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/vendor/css-hamburgers/hamburgers.min.css'>\n"
                <> "<link rel='stylesheet' type='text/css' href='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/vendor/select2/select2.min.css'>\n"
                <> "<link rel='stylesheet' type='text/css' href='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/css/util.css'>\n"
                <> "<link rel='stylesheet' type='text/css' href='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/css/main.css'>\n"
                <> "</head>\n"
                <> "<body>\n"
                <> "<div class='limiter'>\n"
                <> "<div class='container-login100'>\n"
                <> "<div class='wrap-login100'>\n"
                <> "<div class='login100-pic js-tilt' data-tilt>\n"
                <> "<img src='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/images/peaky-login.png' alt='IMG'>\n"
                <> "</div>\n"
                <> "<form class='login100-form validate-form' method='GET' href='#'>\n"
                <> "<span class='login100-form-title'>\n"
                <> "Sing in\n"
                <> "</span>\n"
                <> "<div class='wrap-input100 validate-input' data-validate = 'Valid email is required: ex@abc.xyz'>\n"
                <> "<input class='input100' type='text' name='user-login' placeholder='Login'>\n"
                <> "<span class='focus-input100'></span>\n"
                <> "<span class='symbol-input100'>\n"
                <> "<i class='fa fa-envelope' aria-hidden='true'></i>\n"
                <> "</span>\n"
                <> "</div>\n"
                <> "<div class='wrap-input100 validate-input' data-validate = 'Password is required'>\n"
                <> "<input class='input100' type='password' name='password-login' placeholder='Password'>\n"
                <> "<span class='focus-input100'></span>\n"
                <> "<span class='symbol-input100'>\n"
                <> "<i class='fa fa-lock' aria-hidden='true'></i>\n"
                <> "</span>\n"
                <> "</div>\n"
                <> "<div class='container-login100-form-btn'>\n"
                <> "<button class='login100-form-btn'>\n"
                <> "Login\n"
                <> "</button>\n"
                <> "</div>\n"
                <> "<div class='text-center p-t-136'>\n"
                <> "<a class='txt2' href='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/tasks.cgi/signup'>\n"
                <> "Create your Account\n"
                <> "<i class='fa fa-long-arrow-right m-l-5' aria-hidden='true'></i>\n"
                <> "</a>\n"
                <> "</div>\n"
                <> "</form>\n"
                <> "</div>\n"
                <> "</div>\n"
                <> "</div>\n"
                <> "<script src='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/vendor/jquery/jquery-3.2.1.min.js'></script>\n"
                <> "<script src='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/vendor/bootstrap/js/popper.js'></script>\n"
                <> "<script src='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/vendor/bootstrap/js/bootstrap.min.js'></script>\n"
                <> "<script src='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/vendor/select2/select2.min.js'></script>\n"
                <> "<script src='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/vendor/tilt/tilt.jquery.min.js'></script>\n"
                <> "<script >\n"
                <> "$('.js-tilt').tilt({\n"
                <> "scale: 1.1\n"
                <> "})\n"
                <> "</script>\n"
                <> "<script src='js/main.js'></script>\n"
                <> "</body>\n"
                <> "</html>\n"
        in html3
-- ****************************************************************

signupHtml :: Text -> Text
signupHtml appRoot =
        let html4 = "<!DOCTYPE html>\n"
                <> "<html lang='en' >\n"
                <> "<head>\n"
                <> "    <title>Sign up</title>\n"
                <> "    <meta charset='UTF-8' >\n"
                <> "<meta name='viewport' content='width=device-width, initial-scale=1'>\n"
                <> "<link rel='icon' type='image/png' href='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/images/icons/hs-icon.png'/>\n"
                <> "<link rel='stylesheet' type='text/css' href='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/vendor/bootstrap/css/bootstrap.min.css'>\n"
                <> "<link rel='stylesheet' type='text/css' href='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/fonts/font-awesome-4.7.0/css/font-awesome.min.css'>\n"
                <> "<link rel='stylesheet' type='text/css' href='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/vendor/animate/animate.css'>\n"
                <> "<link rel='stylesheet' type='text/css' href='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/vendor/css-hamburgers/hamburgers.min.css'>\n"
                <> "<link rel='stylesheet' type='text/css' href='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/vendor/select2/select2.min.css'>\n"
                <> "<link rel='stylesheet' type='text/css' href='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/css/util.css'>\n"
                <> "<link rel='stylesheet' type='text/css' href='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/css/main.css'>\n"
                <> "</head>\n"
                <> "<body>\n"
                <> "<div class='limiter'>\n"
                <> "<div class='container-login100'>\n"
                <> "<div class='wrap-login100'>\n"
                <> "<div class='login100-pic js-tilt' data-tilt>\n"
                <> "<img src='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/images/peaky-signup.png' alt='IMG'>\n"
                <> "</div>\n"
                <> "<form class='login100-form validate-form' method=POST action='#'>\n"
                <> "<span class='login100-form-title'>\n"
                <> "Sign up\n"
                <> "</span>\n"
                <> "<div class='wrap-input100 validate-input' data-validate = 'Valid email is required: ex@abc.xyz'>\n"
                <> "<input class='input100' type='text' name='user-signup' placeholder='Login'>\n"
                <> "<span class='focus-input100'></span>\n"
                <> "<span class='symbol-input100'>\n"
                <> "<i class='fa fa-envelope' aria-hidden='true'></i>\n"
                <> "</span>\n"
                <> "</div>\n"
                <> "<div class='wrap-input100 validate-input' data-validate = 'Password is required'>\n"
                <> "<input class='input100' type='password' name='password-signup' placeholder='Password'>\n"
                <> "<span class='focus-input100'></span>\n"
                <> "<span class='symbol-input100'>\n"
                <> "<i class='fa fa-lock' aria-hidden='true'></i>\n"
                <> "</span>\n"
                <> "</div>\n"
                <> "<div class='container-login100-form-btn'>\n"
                <> "<button class='login100-form-btn'>\n"
                <> "Register\n"
                <> "</button>\n"
                <> "</div>\n"
                <> "<div class='text-center p-t-136'>\n"
                <> "<a class='txt2' href='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/tasks.cgi/login'>\n"
                <> "Back to login\n"
                <> "<i class='fa fa-long-arrow-right m-l-5' aria-hidden='true'></i>\n"
                <> "</a>\n"
                <> "</div>\n"
                <> "</form>\n"
                <> "</div>\n"
                <> "</div>\n"
                <> "</div>\n"
                <> "<script src='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/vendor/jquery/jquery-3.2.1.min.js'></script>\n"
                <> "<script src='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/vendor/bootstrap/js/popper.js'></script>\n"
                <> "<script src='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/vendor/bootstrap/js/bootstrap.min.js'></script>\n"
                <> "<script src='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/vendor/select2/select2.min.js'></script>\n"
                <> "<script src='http://soft0.upc.edu/~ldatusr21/practica3/Miquel/project-p3/src/haskell-3/vendor/tilt/tilt.jquery.min.js'></script>\n"
                <> "<script >\n"
                <> "$('.js-tilt').tilt({\n"
                <> "scale: 1.1\n"
                <> "})\n"
                <> "</script>\n"
                <> "<script src='js/main.js'></script>\n"
                <> "</body>\n"
                <> "</html>\n"
        in html4


-- ****************************************************************
pageHtml :: [(TaskId, Task)] -> Maybe Text-> Text
pageHtml tasks mbuser =
    let html1 =    "<!DOCTYPE html>\n"
                <> "<html><head>\n"
                <> "  <title>Tasques</title>\n"
                <> "  <link rel='stylesheet' type='text/css' href='https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css'>\n"
                <> "</head><body><div class='container-fluid'>\n"
                <> "<h1>Tasques</h1>\n"
                <> (if mbuser == Nothing then
                        "<a href=\"http://soft0.upc.edu/~ldatusr21/public_html/practica3/Miquel/tasks.cgi/login\">Login</a> " 
                    else "Hola " <> fromJust mbuser <> "<a href=\"http://soft0.upc.edu/~ldatusr21/public_html/practica3/Miquel/tasks.cgi/logout\" style=\"float: right;\">Logout</a>\n")
                
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
                <> "    <button type='submit' class='btn btn-primary' name='mark'>Marca FET</button>\n"
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

-- ********************************************************

