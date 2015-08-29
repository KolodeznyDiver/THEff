{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# # LANGUAGE ScopedTypeVariables #-} 
module Main where

import Control.THEff
import Control.THEff.Validator

mkEff "MyVldtr"   ''Validator   ''Float     ''NoEff

test1 :: Int -> Either Float Float -- return Left if out of range 
test1 i = runMyVldtr (validator (<30)) $
                    chk $ pi ^ i

test2 :: Int -> Float -- If value is out of range, it is set on the border of the range. 
-- test2 i = right $ runMyVldtr (range 0 30) $ chk $ pi ^ i
test2 i = withRange runMyVldtr 0 30 $ chk $ pi ^ i
          
main:: IO ()
main = do
    print $ test1 2
    print $ test1 3
    print $ test2 2
    print $ test2 3
