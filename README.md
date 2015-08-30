# THEff
  This package implements effects, as alternative to monad
  transformers. Actually, the effects themselves are created without 
  the use of TH, but the binding of nested effects described by 
  mkEff splice. 
  For more information about extensible effects , see the original paper at
  <http://okmij.org/ftp/Haskell/extensible/exteff.pdf>.
  But, this package is significantly different from the original.
  It uses a chains of ordinary GADTs created by TH. 
  No Typeable, unsafe... , ExistentialQuantification ...

