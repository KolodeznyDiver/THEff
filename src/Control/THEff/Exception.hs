{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Control.THEff.Exception (
                      -- * Overview 
-- |
-- > {-# LANGUAGE KindSignatures #-}
-- > {-# LANGUAGE FlexibleInstances #-}
-- > {-# LANGUAGE MultiParamTypeClasses #-}
-- > {-# LANGUAGE TemplateHaskell #-}
-- > 
-- > import Control.THEff
-- > import Control.THEff.Exception
-- >  
-- > mkEff "MyException" ''Except    ''String   ''NoEff
-- > 
-- > safeSqrt :: Float -> Eff (MyException m Float) Float
-- > safeSqrt x =  do
-- >     throwIf (x<0) "The argument must be non-negative."
-- >     return $ sqrt x 
--    
-- >>> runMyException id $ safeSqrt 4
-- Right 2.0
--
-- >>> runMyException id $ safeSqrt (-1)
-- Left "The argument must be non-negative." 

                      -- * Types and functions used in mkEff
                            Except'
                          , Except(..) 
                          , ExceptArgT
                          , ExceptResT
                          , effExcept
                          , runEffExcept 
                      -- * Functions that use this effect                         
                          , throwExc
                          , throwIf
                          ) where

import Control.THEff

-- | Actually, the effect type
--  - __/v/__ - Type - the parameter of the effect.
--  - __/e/__ - mkEff generated type.
newtype Except' v e = Except' v

-- | Type implements link in the chain of effects.
--   Constructors must be named __/{EffectName}{Outer|WriterAction|WriterResult}/__
--   and have a specified types of fields.
-- - __/m/__ - Or Monad (if use the 'Lift') or phantom type - stub (if used 'NoEff').
-- - __/o/__ - Type of outer effect.
-- - __/a/__ - The result of mkEff generated runEEEE... function.
data Except (m:: * -> *) e o v a = ExceptOuter (o m e)
                                 | ExceptAction (Except' v e)    
                                 | ExceptResult a

-- | Type of fourth argument of runEffExcept and first argument of runEEEE. 
type ExceptArgT v = (v -> v)

-- | Result type of runEEEE.  
type ExceptResT r v = Either v r

-- | This function is used in the 'mkEff' generated runEEEE functions and typically 
-- in effect action functions. Calling the effect action.
effExcept:: EffClass Except' v e => Except' v r -> Eff e r
effExcept (Except' v) = effAction $ \_ -> Except' v
    
-- | The main function of the effect implementing. 
-- This function is used in the 'mkEff' generated runEEEE functions. 
runEffExcept :: forall (t :: * -> *) (u :: (* -> *) -> * -> *) (m :: * -> *) 
             z v (m1 :: * -> *) e (o :: (* -> *) -> * -> *) w a r. Monad m =>
    (u t r -> (r -> m (ExceptResT z v)) -> m (ExceptResT z v)) -- ^ The outer effect function
 -> (Except m1 e o w a -> r) -- ^ The chain of effects link wrapper.
 -> (r -> Except t r u v z)  -- ^ The chain of effects link unwrapper.
 -> ExceptArgT v -- ^ The argument of effect. Except value map. Usualy __/id/__.
 -> Eff r a
 -> m  (ExceptResT z v)
runEffExcept outer to un f m = loop $ runEff m (to . ExceptResult)
  where 
    loop = select . un where
        select (ExceptOuter g)  = outer g loop
        select (ExceptAction (Except' v)) = return $ Left $ f v
        select (ExceptResult r) = return $ Right r

-- | Throw effect specific exception.
throwExc :: EffClass Except' v e => v -> Eff e ()
throwExc = effExcept . Except' 

-- | Throw effect specific exception if first argument is True.
throwIf :: EffClass Except' v e => Bool -> v -> Eff e ()
throwIf True  s = throwExc s
throwIf False _ = return ()    
