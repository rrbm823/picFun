{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Store

import Miso
import Miso.Subscription.Keyboard
import Model
import View
import Action


-- | Entry point for a miso application
main :: IO ()
main = do
  startApp App { initialAction = Begin -- initial action to be executed on application load
                 , model  = emptyModel           -- initial model
                 , update = updateModel          -- update function
                 , view   = viewModel            -- view function
                 , events = defaultEvents        -- default delegated events
                 , subs   = [arrowsSub GetArrows
                            , mouseSub HandleMouse
                            ]
                 , mountPoint = Nothing
                 }
