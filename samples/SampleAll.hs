{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-} 
module Main where

import Control.THEff
import Control.THEff.Reader
import Control.THEff.State
import Control.THEff.Exception
import Control.THEff.Catch
import Control.THEff.Writer
import Control.THEff.Fresh
import Control.THEff.Validator
import System.Environment (getProgName)
import Control.Monad 
import Data.Either
import GHC.Float 

mkEff "MyReader"    ''Reader    ''Int       ''Lift
mkEff "SomeState"   ''State     ''Bool      ''MyReader
mkEff "OtherRdr"    ''Reader    ''Float     ''SomeState
mkEff "MyWriter"    ''Writer    ''String    ''OtherRdr
mkEff "MyException" ''Except    ''String    ''MyWriter
mkEff "MyCatch"     ''Catch     ''Int       ''MyException
mkEff "UnicalChar"  ''Fresh     ''Char      ''MyCatch
mkEff "MyVldtr"     ''Validator ''Float     ''UnicalChar

main:: IO ()
main = do
    r <- runMyReader 100 
       $ runSomeState False
       $ runOtherRdr pi 
       $ runMyWriter 
       $ runMyException id 
       $ runMyCatch (("Error code : " ++).show) 
       $ runUnicalChar 'A' $ do
            i :: Int <- asks (*2)                    -- MyReader 
            f :: Float <- ask                        -- OtherRdr
            tell "ProgName="                         -- MyWriter
            b <- get                                 -- SomeState
            put $ not b                              -- SomeState 
            when b $ throwExc "Throw!"               -- MyException 
--            unless b $ throwCtch (123::Int)        -- MyCatch 
            lift $ putStrLn "print from effect!"     -- Lift  
            c1 <- fresh                              -- UnicalChar
            lift $ putStrLn $ "c1=" ++ [c1]          -- Lift  
            c2 <- fresh                              -- UnicalChar 
            lift $ putStrLn $ "c2=" ++ [c2]          -- Lift  
            r <- runMyVldtr (validator (<1000)) $ do
                s <- lift getProgName                 -- Lift
                tell s                                -- MyWriter
                lift $ putStrLn $ "ProgName is " ++ s -- Lift 
                x <- chk $ fromIntegral i * f         -- MyVldtr  
                return $ float2Double x
            return $ either ((++ " is out of range") . show) show r
    print r
