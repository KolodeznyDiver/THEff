{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

module Control.THEff.Reader.Strict (
                      -- * Overview 
-- |
-- This version builds its output strictly; for a lazy version with
-- the same interface, see "Control.THEff.Reader".
--
-- > {-# LANGUAGE KindSignatures #-}
-- > {-# LANGUAGE FlexibleInstances #-}
-- > {-# LANGUAGE MultiParamTypeClasses #-}
-- > {-# LANGUAGE TemplateHaskell #-}
-- > 
-- > import Control.THEff
-- > import Control.THEff.Reader.Strict
-- > 
-- > mkEff "CharReader"   ''Reader    ''Char       ''NoEff
-- > mkEff "StrReader"    ''Reader    ''String     ''CharReader
-- > 
-- > main:: IO ()
-- > main = putStrLn $ runCharReader 'T' $ runStrReader "est" $ do
-- >             c <- ask
-- >             s <- ask
-- >             return $ c:s
-- 
-- __/Output :/__ \"Test\"

                      -- * Types and functions used in mkEff
                              Reader' 
                            , Reader(..)
                            , ReaderArgT
                            , ReaderResT
                            , effReader
                            , runEffReader
                      -- * Functions that use this effect                         
                            , ask
                            , asks
                            ) where

import Control.THEff

-- | Actually, the effect type
--  - __/v/__ - Type - the parameter of the effect.
--  - __/e/__ - mkEff generated type.
newtype Reader' v e = Reader' (v -> e)

-- | Type implements link in the chain of effects.
--   Constructors must be named __/{EffectName}{Outer|WriterAction|WriterResult}/__
--   and have a specified types of fields.
-- - __/m/__ - Or Monad (if use the 'Lift') or phantom type - stub (if used 'NoEff').
-- - __/o/__ - Type of outer effect.
-- - __/a/__ - The result of mkEff generated runEEEE... function.
data Reader (m:: * -> *) e o v a = ReaderOuter (o m e)
                                 | ReaderAction (Reader' v e)
                                 | ReaderResult a 

-- | Type of fourth argument of runEffReader and first argument of runEEEE. 
type ReaderArgT v = v

-- | Result type of runEEEE.  
type ReaderResT r = r

-- | This function is used in the 'mkEff' generated runEEEE functions and typically 
-- in effect action functions. Calling the effect action.
effReader:: EffClass Reader' v e => Reader' v r -> Eff e r
effReader (Reader' g) = effAction $ \k -> Reader' (k . g)

-- | The main function of the effect implementing. 
-- This function is used in the 'mkEff' generated runEEEE functions. 
runEffReader :: forall (t :: * -> *) (u :: (* -> *) -> * -> *) (m :: * -> *) a v 
            (m1 :: * -> *) e (o :: (* -> *) -> * -> *) w a1 r. Monad m =>
    (u t r -> (r -> m (ReaderResT a)) -> m (ReaderResT a)) -- ^ The outer effect function
 -> (Reader m1 e o w a1 -> r) -- ^ The chain of effects link wrapper.
 -> (r -> Reader t r u v a)  -- ^ The chain of effects link unwrapper.
 -> ReaderArgT v -- ^ The initial value of argument of effect.
 -> Eff r a1
 -> m (ReaderResT a)
runEffReader outer to un !v m = loop $ runEff m (to . ReaderResult)
    where
        loop = select . un
        select (ReaderOuter f)  = outer f loop
        select (ReaderAction (Reader' g)) = loop $ g v
        select (ReaderResult r) = return r

-- | Get reader value
ask :: EffClass Reader' v e => Eff e v
ask = effReader $ Reader' id

-- | Get and convert the value of the reader
asks :: EffClass Reader' r e => (r -> v) -> Eff e v
asks f = effReader $ Reader' f
