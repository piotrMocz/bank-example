-- 4. A better version of the banking system, with newtypes, Lens and typeclasses.
-- Summary of problems:
-- * A lot of information is implicit, not encoded in the API,
--   but somewhere in the logic:
--   * USD is assumed as the internal currency
--   (*) User needs to rely on the docs to know what the
--       elements of the tuples are (still for the transactions themselves).
-- * The system lacks a lot of features that a banking system should have.
-- (*) The code is not as verbose, but still not ideal.

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Bank4 where

import           Control.Lens
import           Data.Map     (Map, (!))
import qualified Data.Map     as Map


-- === Currencies === --

-- Conversions between currencies encoded in a type-safe manner.
-- Note that now instead of doing an unsafe map lookup, we can
-- get the compiler to choose the right rate for us!
newtype Dollar = Dollar Double deriving Show
newtype Zloty  = Zloty  Double deriving Show

-- In general, multi-param typeclasses with fundeps are now falling
-- out of fashion: you might want to consider using a type family here.
-- It does convey the point just fine, though.
class Convertible a b | a -> b where
    convert :: a -> b

instance Convertible Dollar Zloty where
    convert (Dollar d) = Zloty $ d * 3.64

instance Convertible Zloty Dollar where
    convert (Zloty z) = Dollar $ z * 0.27


-- === Data === --

newtype FromAddr = FromAddr String deriving Show
newtype ToAddr   = ToAddr   String deriving Show


-- | The bank state, but now without the map: it is all
-- encoded in our type classes. This is still not ideal:
-- the transactions are recorded as tuples and tuples don't
-- carry much information (unlike, for example, data types).
data BankState = BankState
    { _transactions    :: [(FromAddr, ToAddr, Dollar)]
    } deriving Show
makeLenses ''BankState


-- === Helper methods === --

emptyState :: BankState
emptyState = BankState []


-- === API === --

-- The logic is now considerably safer, albeit at a cost of making the type
-- signature a bit more involved. That said, the logic still seems very
-- primitive: we're pushing the 'BankState' struct though the whole
-- application. It seems like something easily abstracted away.
wireMoney :: Convertible a Dollar
          => FromAddr -> ToAddr -> a -> BankState -> BankState
wireMoney fromAddr toAddr amount = transactions %~ (transaction :)
    where transaction = (fromAddr, toAddr, amountUSD)
          amountUSD   = convert amount
