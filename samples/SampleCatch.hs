{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.THEff
import Control.THEff.Catch

mkEff "MyCatch"     ''Catch     ''Float       ''NoEff

foo:: Float -> Eff (MyCatch m String) String
foo x = do
    throwCtchIf  x (==0)
    return $ "1/" ++ show x ++ " = " ++ (show $ 1 / x)
    
hndlr :: Float -> String
hndlr x = "Error : x=" ++ show x

main:: IO ()
main = do
    print $ runMyCatch hndlr $ foo 4
    print $ runMyCatch hndlr $ foo 0
