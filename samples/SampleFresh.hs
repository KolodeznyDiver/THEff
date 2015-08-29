{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.THEff
import Control.THEff.Fresh

mkEff "UnicalChar"  ''Fresh     ''Char      ''NoEff

main:: IO ()
main = putStrLn $ runUnicalChar 'A' $ do
            a <- fresh
            b <- fresh
            c <- fresh
            return $ a:b:[c]
