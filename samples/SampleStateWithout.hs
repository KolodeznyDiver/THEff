{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-} 
module Main where

import Control.THEff
import Control.THEff.State

mkEff "Example1"   ''State     ''Int      ''NoEff

main:: IO ()
main = print $ withoutState runExample1 123 $ do
                    modify ((1 :: Int) +)
                    return "Test"
