{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

module Control.THEff.State.Strict (
                      -- * Overview 
-- |
-- This version builds its output strictly; for a lazy version with
-- the same interface, see "Control.THEff.State".
--
-- > {-# LANGUAGE KindSignatures #-}
-- > {-# LANGUAGE FlexibleInstances #-}
-- > {-# LANGUAGE MultiParamTypeClasses #-}
-- > {-# LANGUAGE TemplateHaskell #-}
-- > 
-- > import Control.THEff
-- > import Control.THEff.State.Strict
-- > 
-- > mkEff "Example1"   ''State     ''Int      ''NoEff
-- > mkEff "Example2"   ''State     ''Float    ''Example1
-- > 
-- > main:: IO ()
-- > main = print $ runExample1 123 
-- >              $ runExample2 pi $ do
-- >                     i <- get
-- >                     modify ((1 :: Int) +)
-- >                     put $ i * (2 :: Float)
-- >                     return  $ show i 
-- 
-- __/Output :/__ (("3.1415927",6.2831855),124)

                      -- * Types and functions used in mkEff
                            State'
                          , State(..) 
                          , StateArgT
                          , StateResT
                          , effState
                          , runEffState 
                      -- * Functions that use this effect                         
                          , get
                          , put
                          , modify
                      -- * Helper functions
                          , stateOnly
                          , withoutState
                          ) where

import Control.THEff

-- | Actually, the effect type
--  - __/v/__ - Type - the parameter of the effect.
--  - __/e/__ - mkEff generated type.
data State' v e = State' (v -> v) (v -> e)  

-- | Type implements link in the chain of effects.
--   Constructors must be named __/{EffectName}{Outer|WriterAction|WriterResult}/__
--   and have a specified types of fields.
-- - __/m/__ - Or Monad (if use the 'Lift') or phantom type - stub (if used 'NoEff').
-- - __/o/__ - Type of outer effect.
-- - __/a/__ - The result of mkEff generated runEEEE... function.
data State (m:: * -> *) e o v a = StateOuter (o m e)
                                | StateAction (State' v e)    
                                | StateResult a

-- | Type of fourth argument of runEffState and first argument of runEEEE. 
type StateArgT v = v

-- | Result type of runEEEE.  
type StateResT r v = (r, v)

-- | This function is used in the 'mkEff' generated runEEEE functions and typically 
-- in effect action functions. Calling the effect action.
effState:: EffClass State' v e => State' v r -> Eff e r
effState (State' f g) = effAction $ \k -> State' f (k . g)

-- | The main function of the effect implementing. 
-- This function is used in the 'mkEff' generated runEEEE functions. 
runEffState :: forall (t :: * -> *) (u :: (* -> *) -> * -> *) (m :: * -> *) 
             z v (m1 :: * -> *) e (o :: (* -> *) -> * -> *) w a r. Monad m =>
    (u t r -> (r -> m (StateResT z v)) -> m (StateResT z v)) -- ^ The outer effect function
 -> (State m1 e o w a -> r) -- ^ The chain of effects link wrapper.
 -> (r -> State t r u v z)  -- ^ The chain of effects link unwrapper.
 -> StateArgT v -- ^ The initial value of argument of effect.
 -> Eff r a
 -> m  (StateResT z v)
runEffState outer to un v m = loop v $ runEff m (to . StateResult)
  where 
    loop !s = select . un where
        select (StateOuter f)  = outer f (loop s)
        select (StateAction (State' t k)) = let s' = t s in loop s' (k s')
        select (StateResult r) = return (r,s)

-- | Get state value
get :: EffClass State' v e => Eff e v
get = effState $ State' id id
    
-- | Put state value
put :: EffClass State' v e => v -> Eff e ()
put !s = modify $ const s
    
-- | Modify state value
modify :: EffClass State' v e => (v -> v) -> Eff e ()
modify f = effState $ State' f (const ())
 
-- | @ stateOnly runExample1 123 === snd (runExample1 123) @
stateOnly :: forall v e r t. 
    (t -> e -> (r, v)) -- ^ State effect runEEEE function
 -> t -- ^ The initial value of argument of effect. 
 -> e -- ^ Eff (MyState m ...) ... 
 -> v
stateOnly f v = snd . (f v)

-- | @ withoutState runExample1 123 === fst (runExample1 123) @
withoutState :: forall v e r t. 
    (t -> e -> (r, v)) -- ^ State effect runEEEE function
 -> t -- ^ The initial value of argument of effect. 
 -> e -- ^ Eff (MyState m ...) ... 
 -> r
withoutState f v = fst . (f v)
