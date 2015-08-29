{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# # LANGUAGE ScopedTypeVariables #-} 
module Main where

import Control.THEff
import Control.THEff.Reader

mkEff "CharReader"   ''Reader    ''Char       ''NoEff
mkEff "StrReader"    ''Reader    ''String     ''CharReader

main:: IO ()
main = putStrLn $ runCharReader 'T' $ runStrReader "est" $ do
            c <- ask
            s <- ask
            return $ c:s
