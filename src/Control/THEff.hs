{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE GADTs #-} 
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Control.THEff
Description : Main module of TH Eff package. 
Copyright   : (c) Kolodezny Diver, 2015
License     : GPL-3
Maintainer  : kolodeznydiver@gmail.com
Stability   : experimental
Portability : Portable
-}

module Control.THEff (
                      -- * Overview 
-- |
--   This package implements effects, as alternative to monad
--   transformers. Actually, the effects themselves are created without 
--   the use of TH, but the binding of nested effects described by 
--   mkEff splice. For example.
--   
-- > {-# LANGUAGE KindSignatures #-}
-- > {-# LANGUAGE FlexibleInstances #-}
-- > {-# LANGUAGE MultiParamTypeClasses #-}
-- > {-# LANGUAGE TemplateHaskell #-}
-- > {-# LANGUAGE ScopedTypeVariables #-} 
-- > 
-- > import Control.THEff
-- > import Control.THEff.Reader
-- > import Control.THEff.State
-- >
-- > mkEff "MyReader"    ''Reader    ''Int       ''Lift
-- > mkEff "SomeState"   ''State     ''Bool      ''MyReader
-- > mkEff "OtherRdr"    ''Reader    ''Float     ''SomeState
-- > 
-- > main:: IO ()
-- > main = do
-- >     r <- runMyReader  100 
-- >        $ runSomeState False
-- >        $ runOtherRdr  pi  $ do
-- >             i :: Int   <- ask                    -- MyReader 
-- >             f :: Float <- ask                    -- OtherRdr
-- >             b <- get                             -- SomeState
-- >             put $ not b                          -- SomeState 
-- >             lift $ putStrLn "print from effect!" -- Lift  
-- >             return $ show $ fromIntegral i * f 
-- >     print r
--  
--  For more information about extensible effects , see the original paper at
--  <http://okmij.org/ftp/Haskell/extensible/exteff.pdf>.
--  But, this package is significantly different from the original.
--  It uses a chains of ordinary GADTs created by TH. 
--  No Typeable, unsafe... , ExistentialQuantification ...
--  
--  Note. Further, wherever referred to  __/runEEEE/__ is meant `mkEff' generated function, e.g.
--  runMyReader, runSomeState, runOtherRdr .
-- 
--  See more in samples/*.hs

                      -- * Base THEff support
                      mkEff
                    , Eff(..)
                    , EffClass(..)
                      -- * No monadic start effect 
                    , NoEff(..)
                    , effNoEff
                    , runNoEff
                      -- * Monadic start effect
                    , Lift'(..)
                    , EffClassM(..)
                    , lift
                    , Lift(..)
                    , runLift
                     )  where

import Control.THEff.TH.Internal(mkEff)

-- | The Monad of effects
newtype Eff w a = Eff {runEff :: (a -> w) -> w}

instance Functor (Eff w) where
      fmap f (Eff g) = Eff $ \k -> g (k . f)

instance Applicative (Eff w) where
    pure = return
    Eff f <*> Eff g = Eff $ \k -> f (\v -> g (k . v))
      
instance Monad (Eff w) where
    return x = Eff $ \k -> k x
    m >>= f =  Eff $ \k -> runEff m (\v -> runEff (f v) k)

-- | Helper class to transfer the action effects by chain. 
--   Instances of this class are created in @mkEff@.
class EffClass w v e where
    effAction:: ((r -> e) -> w v e) -> Eff e r 
    effAction f = Eff $ \k -> toEff $ f k
    toEff:: w v e -> e

-- | The first effect in a chain of effects not use monads.
--   The chain of effects should start or that type, or @Lift@ (See below.)
newtype NoEff (m:: * -> *) a = NoEff { unNoEff :: a}

-- | This function is used in the 'mkEff' generated runEEEE... functions. 
-- @effNoEff _ = error "THEff: Attempting to call the effect NoEff that does not have any actions!"@
effNoEff :: a -> b
effNoEff _ = error "THEff: Attempting to call the effect NoEff that does not have any actions!"

-- | This function is used in the 'mkEff' generated runEEEE... functions. 
--   Do not use it alone.
runNoEff :: Eff (NoEff m a) a -> a
runNoEff m = unNoEff $ runEff m NoEff

-- | Helper data type for transfer the monadic action effects by chain. 
data Lift' m v = forall a. Lift' (m a) (a -> v)

-- | Helper class to transfer the monadic action effects by chain. 
--   Instances of this class are created in @mkEff@.
class EffClassM m e where
    effLift:: Lift' m r -> Eff e r
    effLift (Lift' m g) = Eff $ \k -> toEffM $ Lift' m (k . g)
    -- | 
    toEffM:: Lift' m e -> e

-- | Lift a Monad to an Effect.
lift:: (Monad m, EffClassM m e) => m a -> Eff e a
lift m = effLift $ Lift' m id

-- | The first effect in a chain of monadic effects.
--   The chain of effects should start or that type, or @NoEff@.
data Lift m a = Lift_ (Lift' m (Lift m a))
              | LiftResult a

instance EffClassM m (Lift m a) where
    toEffM = Lift_

-- | This function is used in the @mkEff@ generated runEEEE... functions. 
--   Do not use it alone.
runLift :: Monad m => Eff (Lift m a) a -> m a
runLift e = loop $ runEff e LiftResult
    where
        loop (Lift_ (Lift' m g)) = m >>= loop . g 
        loop (LiftResult r) = return r
