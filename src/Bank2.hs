-- 2. A better version of the banking system, with Lens.
-- Summary of problems:
-- * Unsafe: certain conditions can cause a runtime error.
-- * A lot of information is implicit, not encoded in the API,
--   but somewhere in the logic:
--   * USD is assumed as the internal currency
--   * User needs to rely on the docs to know what the
--     elements of the tuples are
--   * It is very easy to mix things up, for instance: the sender and recipient address.
-- * The system lacks a lot of features that a banking system should have.
-- (*) The code is not as verbose, but still not ideal.
{-# LANGUAGE TemplateHaskell #-}

module Bank2 where

import           Control.Lens
import           Data.Map     (Map, (!))
import qualified Data.Map     as Map

-- === Data === --

data BankState = BankState
    { _transactions    :: [(String, String, Double)]
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
    where rates = bankState ^. conversionRates
          rate  = rates ! (fromCurr, toCurr)   -- still UNSAFE :(

emptyState :: BankState
emptyState = BankState [] baseConversionRates


-- === API === --

-- | The same, very primitive transaction logic as in the `Bank` module.
-- Note, however, how much shorter it has gotten thanks only to the use
-- of the `lens` library.
wireMoney :: String -> String -> Double -> String -> BankState -> BankState
wireMoney fromAddr toAddr amount currency oldState =
    oldState & transactions %~ (transaction :)
    where transaction = (fromAddr, toAddr, amountUSD)
          amountUSD   = convert currency "USD" amount oldState
