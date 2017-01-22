{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Control.THEff.Validator (
                      -- * Overview 
-- |
-- > {-# LANGUAGE KindSignatures #-}
-- > {-# LANGUAGE FlexibleInstances #-}
-- > {-# LANGUAGE MultiParamTypeClasses #-}
-- > {-# LANGUAGE TemplateHaskell #-}
-- > 
-- > import Control.THEff
-- > import Control.THEff.Validator
-- > 
-- > mkEff "MyVldtr"   ''Validator   ''Float     ''NoEff
-- > 
-- > test1 :: Int -> Either Float Float -- return Left if out of range
-- > test1 i = runMyVldtr (validator (<30)) $
-- >                     chk $ pi ^ i
-- > 
-- > test2 :: Int -> Float -- If value is out of range, it is set on the border of the range.
-- > test2 i = withRange runMyVldtr 0 30 $ chk $ pi ^ i
--    
-- >>> test1 2
-- Right 9.869605
--
-- >>> test1 3
-- Left 31.006279
-- 
-- >>> test2 2
-- 9.869605
-- 
-- >>> test2 3
-- 30.0

                      -- * Types and functions used in mkEff
                            Validator'
                          , Validator(..) 
                          , ValidatorArgT
                          , ValidatorResT
                          , effValidator
                          , runEffValidator 
                      -- * Functions that use this effect                         
                          , chk
                      -- ** Functions for substitution in the first argument runEEEEE....
                          , validator
                          , range
                      -- * Helper functions
                          , right
                          , withRange
                          ) where

import Control.THEff

-- | Actually, the effect type
--  - __/v/__ - Type - the parameter of the effect.
--  - __/e/__ - mkEff generated type.
data Validator' v e = Validator' v (v -> e)  

-- | Type implements link in the chain of effects.
--   Constructors must be named __/{EffectName}{Outer|WriterAction|WriterResult}/__
--   and have a specified types of fields.
-- - __/m/__ - Or Monad (if use the 'Lift') or phantom type - stub (if used 'NoEff').
-- - __/o/__ - Type of outer effect.
-- - __/a/__ - The result of mkEff generated runEEEE... function.
data Validator (m:: * -> *) e o v a = ValidatorOuter (o m e)
                                    | ValidatorAction (Validator' v e)    
                                    | ValidatorResult a

-- | Type of fourth argument of runEffValidator and first argument of runEEEE. 
type ValidatorArgT v r = (v -> Either v v)

-- | Result type of runEEEE.  
type ValidatorResT r v = Either v r

-- | This function is used in the 'mkEff' generated runEEEE functions and typically 
-- in effect action functions. Calling the effect action.
effValidator:: EffClass Validator' v e => Validator' v r -> Eff e r
effValidator (Validator' v g) = effAction $ \k -> Validator' v (k . g)

                    
-- | The main function of the effect implementing. 
-- This function is used in the 'mkEff' generated runEEEE functions. 
runEffValidator :: forall (t :: * -> *) (u :: (* -> *) -> * -> *) (m :: * -> *) 
             z v (m1 :: * -> *) e (o :: (* -> *) -> * -> *) w a r. Monad m =>
    (u t r -> (r -> m (ValidatorResT z v)) -> m (ValidatorResT z v)) -- ^ The outer effect function
 -> (Validator m1 e o w a -> r) -- ^ The chain of effects link wrapper.
 -> (r -> Validator t r u v z)  -- ^ The chain of effects link unwrapper.
 -> ValidatorArgT v z -- ^ The argument of effect. A value validator function. 
 -> Eff r a
 -> m  (ValidatorResT z v)
runEffValidator outer to un f m = loop $ runEff m (to . ValidatorResult)
  where 
    loop = select . un where
        select (ValidatorOuter g)  = outer g loop
        select (ValidatorAction (Validator' v k)) = 
                case f v of
                    (Left x)  -> return $ Left x
                    (Right x) -> loop (k x)
        select (ValidatorResult r) = return $ Right r

-- | Check the conditions specified by the first argument of runEEEE
chk :: EffClass Validator' v e => v -> Eff e v
chk v = effValidator $ Validator' v id

-- | validator returns __/Right v/__ if the predicate returns True and __/Left v/__ else.
validator :: 
    (v -> Bool) -- ^ predicate
 -> v           -- ^ value
 -> Either v v  
validator f v | f v = Right v
              | otherwise = Left v
  
-- | If the value is outside this range, `range' sets the value of the range. 
-- Always returns __/Right/__. 
range :: Ord v => 
    v -- ^ The lower limit of the range.
 -> v -- ^ The upper limit of the range
 -> v -- ^ The value
 -> Either v v
range vmin vmax v | vmin>v = Right vmin
                  | v>vmax = Right vmax 
                  | otherwise = Right v


-- | right ~(Right v) = v
right :: Either a b -> b 
right ~(Right v) = v

-- | If the runEEEE return value is out of range, it is set on the border of the range.
withRange :: forall r e l v. Ord v =>
    ((v -> Either v v) -> e -> Either l r) -- ^ Validator effect runEEEE function
 -> v -- ^ The lower limit of the range.
 -> v -- ^ The upper limit of the range.
 -> e -- ^ Eff (MyValidator m ...) ... 
 -> r
withRange f vmin vmax = right . (f (range vmin vmax))
