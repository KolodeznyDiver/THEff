{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Control.THEff.Catch (
                      -- * Overview 
-- |
-- > {-# LANGUAGE KindSignatures #-}
-- > {-# LANGUAGE FlexibleInstances #-}
-- > {-# LANGUAGE MultiParamTypeClasses #-}
-- > {-# LANGUAGE TemplateHaskell #-}
-- > 
-- > import Control.THEff
-- > import Control.THEff.Catch
-- > 
-- > mkEff "MyCatch"     ''Catch     ''Float       ''NoEff
-- > 
-- > foo:: Float -> Eff (MyCatch m String) String
-- > foo x = do
-- >     throwCtchIf  x (==0)
-- >     return $ "1/" ++ show x ++ " = " ++ (show $ 1 / x)
-- >     
-- > hndlr :: Float -> String
-- > hndlr x = "Error : x=" ++ show x
--    
-- >>> runMyCatch hndlr $ foo 4
-- "1/4.0 = 0.25"
--
-- >>> runMyCatch hndlr $ foo 0
-- "Error : x=0.0" 

                      -- * Types and functions used in mkEff
                            Catch'
                          , Catch(..) 
                          , CatchArgT
                          , CatchResT
                          , effCatch
                          , runEffCatch 
                      -- * Functions that use this effect                         
                          , throwCtch
                          ,throwCtchIf
                          ) where

import Control.THEff

-- | Actually, the effect type
--  - __/v/__ - Type - the parameter of the effect.
--  - __/e/__ - mkEff generated type.
newtype Catch' v e = Catch' v

-- | Type implements link in the chain of effects.
--   Constructors must be named __/{EffectName}{Outer|WriterAction|WriterResult}/__
--   and have a specified types of fields.
-- - __/m/__ - Or Monad (if use the 'Lift') or phantom type - stub (if used 'NoEff').
-- - __/o/__ - Type of outer effect.
-- - __/a/__ - The result of mkEff generated runEEEE function.
data Catch (m:: * -> *) e o v a = CatchOuter (o m e)
                                | CatchAction (Catch' v e)    
                                | CatchResult a

-- | Type of fourth argument of runEffCatch and first argument of runEEEE. 
type CatchArgT v r = (v -> r)

-- | Result type of runEEEE.  
type CatchResT r = r

-- | This function is used in the 'mkEff' generated runEEEE functions and typically 
-- in effect action functions. Calling the effect action.
effCatch:: EffClass Catch' v e => Catch' v r -> Eff e r
effCatch (Catch' v) = effAction $ \_ -> Catch' v
    
-- | The main function of the effect implementing. 
-- This function is used in the 'mkEff' generated runEEEE functions. 
runEffCatch :: forall (t :: * -> *) (u :: (* -> *) -> * -> *) (m :: * -> *) 
             z v (m1 :: * -> *) e (o :: (* -> *) -> * -> *) w a r. Monad m =>
    (u t r -> (r -> m (CatchResT z)) -> m (CatchResT z)) -- ^ The outer effect function
 -> (Catch m1 e o w a -> r) -- ^ The chain of effects link wrapper.
 -> (r -> Catch t r u v z)  -- ^ The chain of effects link unwrapper.
 -> CatchArgT v z -- ^ The argument of effect. Checking and/or correction function. 
 -> Eff r a
 -> m  (CatchResT z)
runEffCatch outer to un f m = loop $ runEff m (to . CatchResult)
  where 
    loop = select . un where
        select (CatchOuter g)  = outer g loop
        select (CatchAction (Catch' v)) = return $ f v
        select (CatchResult r) = return r

-- | Throw effect specific exception.
throwCtch :: EffClass Catch' v e => v -> Eff e ()
throwCtch = effCatch . Catch' 

-- | Throw effect specific exception if first argument is True.
throwCtchIf :: EffClass Catch' v e => v -> (v -> Bool) -> Eff e ()
throwCtchIf v f | f v = throwCtch v
                | otherwise = return ()