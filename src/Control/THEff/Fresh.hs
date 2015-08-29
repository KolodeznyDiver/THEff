{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Control.THEff.Fresh (
                      -- * Overview 
-- |
-- > {-# LANGUAGE KindSignatures #-}
-- > {-# LANGUAGE FlexibleInstances #-}
-- > {-# LANGUAGE MultiParamTypeClasses #-}
-- > {-# LANGUAGE TemplateHaskell #-}
-- > {- # LANGUAGE ScopedTypeVariables #-} 
-- > module Main where
-- > 
-- > import Control.THEff
-- > import Control.THEff.Fresh
-- > 
-- > mkEff "UnicalChar"  ''Fresh     ''Char      ''NoEff
-- > 
-- > main:: IO ()
-- > main = putStrLn $ runUnicalChar 'A' $ do
-- >             a <- fresh
-- >             b <- fresh
-- >             c <- fresh
-- >             return $ a:b:[c]
-- 
-- __/Output :/__ ABC

                      -- * Types and functions used in mkEff
                            Fresh'
                          , Fresh(..) 
                          , FreshArgT
                          , FreshResT
                          , effFresh
                          , runEffFresh 
                      -- * Functions that use this effect                         
                          , fresh
                          ) where

import Control.THEff

-- | Actually, the effect type
--  - __/v/__ - Type - the parameter of the effect.
--  - __/e/__ - mkEff generated type.
data Fresh' v e = Fresh' (v -> e)  

-- | Type implements link in the chain of effects.
--   Constructors must be named __/{EffectName}{Outer|WriterAction|WriterResult}/__
--   and have a specified types of fields.
-- - __/m/__ - Or Monad (if use the 'Lift') or phantom type - stub (if used 'NoEff').
-- - __/o/__ - Type of outer effect.
-- - __/a/__ - The result of mkEff generated runEEEE... function.
data Fresh (m:: * -> *) e o v a = FreshOuter (o m e)
                                | FreshAction (Fresh' v e)    
                                | FreshResult a

-- | Type of fourth argument of runEffFresh and first argument of runEEEE. 
type FreshArgT v = v
 
-- | Result type of runEEEE.  
type FreshResT r = r

-- | This function is used in the 'mkEff' generated runEEEE functions and typically 
-- in effect action functions. Calling the effect action.
effFresh:: EffClass Fresh' v e => Fresh' v r -> Eff e r
effFresh (Fresh' g) = effAction $ \k -> Fresh' (k . g)
    
-- | The main function of the effect implementing. 
-- This function is used in the 'mkEff' generated runEEEE functions. 
runEffFresh :: forall (t :: * -> *) (u :: (* -> *) -> * -> *) (m :: * -> *) 
             z v (m1 :: * -> *) e (o :: (* -> *) -> * -> *) w a r. (Monad m, Enum v) =>
    (u t r -> (r -> m (FreshResT z)) -> m (FreshResT z)) -- ^ The outer effect function
 -> (Fresh m1 e o w a -> r) -- ^ The chain of effects link wrapper.
 -> (r -> Fresh t r u v z)  -- ^ The chain of effects link unwrapper.
 -> FreshArgT v -- ^ The initial value of argument of effect.
 -> Eff r a
 -> m  (FreshResT z)
runEffFresh outer to un v m = loop v $ runEff m (to . FreshResult)
  where 
    loop s = select . un where
        select (FreshOuter g)  = outer g (loop s)
        select (FreshAction (Fresh' k)) = (loop $! succ s) (k s)
        select (FreshResult r) = return r

-- | Get a unique value.
fresh :: EffClass Fresh' v e => Eff e v
fresh = effFresh $ Fresh' id
