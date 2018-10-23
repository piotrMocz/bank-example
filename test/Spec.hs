import qualified Bank
import qualified Bank2
import qualified Bank3
import qualified Bank4
import qualified Bank5

import           Control.Lens
import qualified Control.Monad.State as State
import           Test.Hspec
import           Test.QuickCheck


main :: IO ()
main = hspec $ do
    describe "naive bank" .
        it "should record the transactions" $ do
            let bs = Bank.emptyState
                s  = Bank.wireMoney "from" "to" 123.0 "PLN" bs
                n  = length $ Bank.transactions s
            n `shouldBe` 1

    describe "bank with lenses" .
        it "should record the transactions" $ do
            let bs = Bank2.emptyState
                s  = Bank2.wireMoney "from" "to" 123.0 "PLN" bs
                s' = Bank2.wireMoney "to" "from" 321.0 "USD" s
                n  = length $ s' ^. Bank2.transactions
            n `shouldBe` 2

    describe "bank with newtypes" .
        it "should record the transactions" $ do
            let bs   = Bank3.emptyState
                from = Bank3.FromAddr "from"
                to   = Bank3.ToAddr   "to"
                s    = Bank3.wireMoney from to 123.0 "PLN" bs
                n    = length $ s ^. Bank3.transactions
            n `shouldBe` 1

    describe "bank with conversions" .
        it "should record the transactions" $ do
            let bs   = Bank4.emptyState
                from = Bank4.FromAddr "from"
                to   = Bank4.ToAddr   "to"
                s    = Bank4.wireMoney from to (Bank4.Zloty 123.0) bs
                n    = length $ s ^. Bank4.transactions
            n `shouldBe` 1

    describe "bank with state" .
        it "should record the transactions" $ do
            s <- Bank5.runBank $ do
                let from  = Bank5.FromAddr "from"
                    to    = Bank5.ToAddr   "to"
                    trans = Bank5.Transaction from to (Bank5.Zloty 123.0)

                Bank5.wireMoney trans

            let n = length $ s ^. Bank5.transactions
            n `shouldBe` 1
