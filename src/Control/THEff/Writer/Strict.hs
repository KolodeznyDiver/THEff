{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

module Control.THEff.Writer.Strict (
                      -- * Overview 
-- |
-- This version builds its output strictly; for a lazy version with
-- the same interface, see "Control.THEff.Writer".
--
-- > {-# LANGUAGE KindSignatures #-}
-- > {-# LANGUAGE FlexibleInstances #-}
-- > {-# LANGUAGE MultiParamTypeClasses #-}
-- > {-# LANGUAGE TemplateHaskell #-}
-- > 
-- > import Control.THEff
-- > import Control.THEff.Writer.Strict
-- > import Control.Monad(forM_) 
-- > import Data.Monoid
-- > 
-- > type IntAccum = Sum Int
-- > 
-- > mkEff "StrWriter"   ''Writer   ''String    ''NoEff
-- > mkEff "IntWriter"   ''Writer   ''IntAccum  ''StrWriter
-- > 
-- > main:: IO ()
-- > main = putStrLn $ uncurry (flip (++)) $ runStrWriter $ do
-- >             tell "Result"
-- >             (r, Sum v) <- runIntWriter $ do
-- >                 tell "="
-- >                 forM_ [1::Int .. 10]
-- >                     (tell . Sum)
-- >                 return (pi :: Float)
-- >             return $ show $ r * fromIntegral v
-- 
-- __/Output :/__ Result=172.7876
--
-- Note that for the effect `Writer' `mkEff' generates unary function runEEEE. `mkEff' chooses 
-- generation unary or binary function runEEEE based on number of arguments of  effect function. 
-- E.g. runEffWriter.

                      -- * Types and functions used in mkEff
                            Writer'
                          , Writer(..) 
                          , WriterResT
                          , effWriter
                          , runEffWriter 
                      -- * Functions that use this effect                         
                          , tell
                          ) where

import Control.THEff
import Data.Monoid

-- | Actually, the effect type
--  - __/v/__ - Type - the parameter of the effect.
--  - __/e/__ - mkEff generated type.
data Writer' v e = Writer' !v (() -> e)  

-- | Type implements link in the chain of effects.
--   Constructors must be named __/{EffectName}{Outer|WriterAction|WriterResult}/__
--   and have a specified types of fields.
-- - __/m/__ - Or Monad (if use the 'Lift') or phantom type - stub (if used 'NoEff').
-- - __/o/__ - Type of outer effect.
-- - __/a/__ - The result of mkEff generated runEEEE... function.
data Writer (m:: * -> *) e o v a = WriterOuter (o m e)
                                 | WriterAction (Writer' v e)    
                                 | WriterResult a

-- | Result type of runEEEE.  
type WriterResT r v = (r, v)

-- | This function is used in the 'mkEff' generated runEEEE functions and typically 
-- in effect action functions. Calling the effect action.
effWriter:: EffClass Writer' v e => Writer' v r -> Eff e r
effWriter (Writer' v g) = effAction $ \k -> Writer' v (k . g)
 
-- | The main function of the effect implementing. 
-- This function is used in the 'mkEff' generated runEEEE functions. 
runEffWriter :: forall (t :: * -> *) (u :: (* -> *) -> * -> *) (m :: * -> *) 
             z v (m1 :: * -> *) e (o :: (* -> *) -> * -> *) w a r. (Monad m, Monoid v) =>
    (u t r -> (r -> m (WriterResT z v)) -> m (WriterResT z v)) -- ^ The outer effect function
 -> (Writer m1 e o w a -> r) -- ^ The chain of effects link wrapper.
 -> (r -> Writer t r u v z)  -- ^ The chain of effects link unwrapper.
--             -> WriterArgT v  -- unused argument!
 -> Eff r a
 -> m  (WriterResT z v)
runEffWriter outer to un m = loop mempty $ runEff m (to . WriterResult)
  where 
    loop !s = select . un where
        select (WriterOuter g)  = outer g (loop s)
        select (WriterAction (Writer' v k)) = let s' = s `mappend` v -- (f v)
                                              in loop s' (k ())
        select (WriterResult r) = return (r,s)

-- | Add value to monoid. 
tell :: EffClass Writer' v e => v -> Eff e ()
tell !v = effWriter $ Writer' v (const ())
