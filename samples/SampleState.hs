{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-} 
module Main where


import Control.THEff
import Control.THEff.State

mkEff "Example1"   ''State     ''Int      ''NoEff
mkEff "Example2"   ''State     ''Float    ''Example1

main:: IO ()
main = print $ runExample1 123 
             $ runExample2 pi $ do
                    i <- get
                    modify ((1 :: Int) +)
                    put $ i * (2 :: Float)
                    return $ show i 
