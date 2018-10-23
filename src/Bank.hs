-- 1. An initial attempt at the Banking system.
-- Summary of problems:
-- * Unsafe: certain conditions can cause a runtime error.
-- * A lot of information is implicit, not encoded in the API,
--   but somewhere in the logic:
--   * USD is assumed as the internal currency
--   * User needs to rely on the docs to know what the
--     elements of the tuples are
--   * It is very easy to mix things up, for instance: the sender and recipient address.
-- * The code is really verbose.
-- * The system lacks a lot of features that a banking system should have.

module Bank where

import           Data.Map (Map, (!))
import qualified Data.Map as Map


-- === Data === --

-- | The data structure capturing the current state
-- of the bank's system.
data BankState = BankState
    { transactions    :: [(String, String, Double)]   -- ^ (from, to, amount): IMPLICIT, not in the type.
    , conversionRates :: Map (String, String) Double  -- ^ currency pair -> rate
    } deriving Show


-- === Helper methods === --

-- | A mapping between the supported currency pairs
-- and their respective conversion rates. Even though
-- `fromList` is quite costly, it will only get evaluated
-- once (excercise: why?).
-- IMPLICIT: the to- and from- elements of the pair.
baseConversionRates :: Map (String, String) Double
baseConversionRates = Map.fromList
    [ (("PLN", "USD"), 0.27)
    , (("USD", "PLN"), 3.64)]

-- | Convert money from one currency to another, looking
-- up the rates in the global state's map.
-- IMPLICIT: the user can switch `fromCurr` and `toCurr`.
convert :: String -> String -> Double -> BankState -> Double
convert fromCurr toCurr amount bankState = rate * amount
    where rates = conversionRates bankState
          rate  = rates ! (fromCurr, toCurr)   -- UNSAFE :(

-- | Initialize the state of the bank.
emptyState :: BankState
emptyState = BankState [] baseConversionRates


-- === API === --

-- | Record the transfer of money from one address to another.
-- This function is not beautiful: notice how much code it takes
-- just to:
-- 1. read a field of a struct
-- 2. modify it
-- 3. create a new struct with the updated field.
-- This is a very common operation and great abstractions exist, taking
-- care of just that (and many other things).
--
-- Also, note that this logic is very primitive: for instance, it does
-- not record the account balances.
wireMoney :: String -> String -> Double -> String -> BankState -> BankState
wireMoney fromAddr toAddr amount currency oldState = newState
    where transaction      = (fromAddr, toAddr, amountUSD)
          transactions'    = transaction : transactions oldState
          conversionRates' = conversionRates oldState
          -- UNWISE: the code assumes we will be converting all of our
          -- transactions to dollars. The API doesn't reflect that at all.
          amountUSD        = convert currency "USD" amount oldState
          newState         = BankState transactions' conversionRates'
