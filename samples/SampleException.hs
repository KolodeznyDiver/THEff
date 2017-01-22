{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.THEff
import Control.THEff.Exception
--import Control.Monad(when)
 
mkEff "MyException" ''Except    ''String   ''NoEff

safeSqrt :: Float -> Eff (MyException m Float) Float
safeSqrt x =  do
    throwIf (x<0) "The argument must be non-negative."
--    when (x<0) $
--        throwExc "The argument must be non-negative."
    return $ sqrt x 
  
main:: IO ()
main = do
    print $ runMyException id $ safeSqrt 4
    print $ runMyException id $ safeSqrt (-1)
