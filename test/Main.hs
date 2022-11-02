{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import           Prelude
import           Test.QuickCheck             (quickCheck)
import ENCOINS.Core.Bulletproofs.Prove (bulletproof)
import ENCOINS.Core.OnChain (bulletproofSetup)
import ENCOINS.Core.BaseTypes 
import PlutusTx.Prelude (emptyByteString)
import Data.Maybe (fromJust)
import ENCOINS.Core.Bulletproofs.Types (Secret(..), Randomness (Randomness))
import ENCOINS.Core.Bulletproofs.Utils (polarity)

main :: IO ()
main = do
    let params = fromJust $ toGroupElement emptyByteString
        secret = [Secret (F 1231) (F 245)]
        r      = Randomness (F 8786) (map F [1..10]) (map F [351..360]) (F 78315421) (F 7126519) (F 1568502398532986345)
        (v, ins, proof) = bulletproof bulletproofSetup params secret r
    print v
    print ins
    print proof
    print "Finished!"