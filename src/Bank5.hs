-- 5. A better yet version of the banking system, with newtypes,
-- Lens, typeclasses and State monad.
-- Summary of problems:
-- * The system lacks a lot of features that a banking system should have.
-- Notice that we managed to get rid of most problems (and using only
-- a moderate amount of fancy Haskell features).
-- Also notice, that the important changes are not the ones that
-- make the code concise (that can be considered showing off), but
-- the ones that make the API clearly encode what the code does,
-- thus making the libraries maintainable and usable.

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Bank5 where

import           Control.Lens
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State    (MonadState, StateT)
import qualified Control.Monad.State    as State
import           Data.Map               (Map, (!))
import qualified Data.Map               as Map


-- === Currencies === --

newtype Dollar = Dollar Double deriving Show
newtype Zloty  = Zloty  Double deriving Show

class Convertible a b | a -> b where
    convert :: a -> b

instance Convertible Dollar Zloty where
    convert (Dollar d) = Zloty $ d * 3.64

instance Convertible Zloty Dollar where
    convert (Zloty z) = Dollar $ z * 0.275


-- === Data === --

newtype FromAddr = FromAddr String deriving Show
newtype ToAddr   = ToAddr   String deriving Show

-- | A datatype encapsulating a transaction. Moreover,
-- it is parametrised by the currency it is in. It retains
-- all of the information we used to have in the tuple, but
-- adds quite a lot of context and makes the API clearer.
data Transaction a = Transaction
    { _fromAddr :: FromAddr
    , _toAddr   :: ToAddr
    , _amount   :: a
    } deriving Show
makeLenses ''Transaction

-- | The ledger is actually what constitutes the state of
-- the bank system: a record of all transactions in dollars
-- (up till now we only implicitly encoded that all of our transactions
-- are in dollars -- it was not visible in the API).
newtype Ledger = Ledger
    { _transactions :: [Transaction Dollar]
    } deriving Show
makeLenses ''Ledger

-- | A type alias to make the signatures easier to write.
-- NOTE: this is not a new type, it is just a new name.
type BankState a = StateT Ledger IO a


-- === Helper methods === --

emptyState :: Ledger
emptyState = Ledger []


-- === API === --

-- | Yet another tradeoff: we made the logic considerably more
-- concise at the cost of making the types more complicated.
wireMoney :: (Convertible a Dollar, MonadState Ledger m, MonadIO m)
          => Transaction a -> m ()
wireMoney t = do
    let t' = t & amount %~ convert
    State.modify $ transactions %~ (t' :)

-- | The entry point of the whole banking system: the bank operations
-- run in a monad: this is a way of getting in and out of the monad.
runBank :: BankState () -> IO Ledger
runBank = flip State.execStateT emptyState
