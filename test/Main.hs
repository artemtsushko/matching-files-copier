-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  Copyright (c) 2015 Artem Tsushko
-- License     :  BSD3
--
-- Maintainer  :  Artem Tsushko <artemtsushko@gmail.com>
-- Stability   :  provisional
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Main.hs"]


