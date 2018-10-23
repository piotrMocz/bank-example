-- 3. A better version of the banking system, with newtypes and Lens.
-- Summary of problems:
-- * Unsafe: certain conditions can cause a runtime error.
-- * A lot of information is implicit, not encoded in the API,
--   but somewhere in the logic:
--   * USD is assumed as the internal currency
--   * User needs to rely on the docs to know what the
--     elements of the tuples are (still for the currencies).
-- * The system lacks a lot of features that a banking system should have.
-- (*) The code is not as verbose, but still not ideal.

{-# LANGUAGE TemplateHaskell #-}

module Bank3 where

import           Control.Lens
import           Data.Map     (Map, (!))
import qualified Data.Map     as Map

-- === Data === --

-- Newtypes, creating a distinction between the sender address,
-- and recipient address. At runtime, these are exactly the same values
-- (thus no overhead), but the compiler now won't let you switch one for the other!
newtype FromAddr = FromAddr String deriving Show
newtype ToAddr   = ToAddr   String deriving Show

-- | 'BankState' but with the new, type-safe addresses
data BankState = BankState
    { _transactions    :: [(FromAddr, ToAddr, Double)]
    , _conversionRates :: Map (String, String) Double
    } deriving Show
makeLenses ''BankState


-- === Helper methods === --

baseConversionRates :: Map (String, String) Double
baseConversionRates = Map.fromList
    [ (("PLN", "USD"), 0.27)
    , (("USD", "PLN"), 3.64)]

convert :: String -> String -> Double -> BankState -> Double
convert fromCurr toCurr amount bankState = rate * amount
    where rates = bankState ^. conversionRates   -- and still UNSAFE :(
          rate  = rates ! (fromCurr, toCurr)

emptyState :: BankState
emptyState = BankState [] baseConversionRates


-- === API === --

-- | The updated, little bit more safe transaction logic.
wireMoney :: FromAddr -> ToAddr -> Double -> String -> BankState -> BankState
wireMoney fromAddr toAddr amount currency oldState =
    oldState & transactions %~ (transaction :)
    where transaction = (fromAddr, toAddr, amountUSD)
          amountUSD   = convert currency "USD" amount oldState
