{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.THEff
import Control.THEff.Writer
import Control.Monad(forM_) 
import Data.Monoid

type IntAccum = Sum Int

mkEff "StrWriter"   ''Writer   ''String    ''NoEff
mkEff "IntWriter"   ''Writer   ''IntAccum  ''StrWriter

main:: IO ()
main = putStrLn $ uncurry (flip (++)) $ runStrWriter $ do
            tell "Result"
            (r, Sum v) <- runIntWriter $ do
                tell "="
                forM_ [1::Int .. 10]
                    (tell . Sum)
                return (pi :: Float)
            return $ show $ r * fromIntegral v 