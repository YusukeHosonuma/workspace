#!/usr/bin/env stack
-- stack --resolver lts-7.0 --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}
import Turtle

echoModified :: Text -> IO ()
echoModified fname = do
    let file = fromText fname
    modified <- datefile file
    printf ("File: "%s%" Modified: "%s%"\n") fname (repr modified)
    return ()

main = do
    args <- arguments
    mapM echoModified args
    return ()
