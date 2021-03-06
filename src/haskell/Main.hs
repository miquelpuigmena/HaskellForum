
{-# LANGUAGE OverloadedStrings #-}

module Main
where
import App

import Network.Wai
import Network.Wai.Handler.CGI(run)
import Network.Wai.Middleware.Approot(envFallbackNamed)

import Control.Exception

-- ****************************************************************

main :: IO ()
main = do
    r <- try makeApp
    case r of
        Right app -> do
            -- CGI adapter
            appRootMWare <- envFallbackNamed "SCRIPT_NAME"
            run $ appRootMWare app
        Left exc -> do
            -- Exception on initialization
            putStrLn "Status: 500 Internal Server Error"
            putStrLn "Content-Type: text/plain"
            putStrLn ""
            putStrLn "Exception on initialization (while excution of 'makeApp'): "
            putStrLn $ "    " ++ show (exc :: SomeException)

main_test :: IO ()
main_test = do
    r <- try makeApp
    case r of
        Right app -> do
            putStrLn "Content-Type: text/plain"
            putStrLn ""
            putStrLn "OK"
        Left exc -> do
            putStrLn "Status: 500 Internal Server Error"
            putStrLn "Content-Type: text/plain"
            putStrLn ""
            putStrLn "TEST"
            putStrLn $ "    " ++ show (exc :: SomeException)

