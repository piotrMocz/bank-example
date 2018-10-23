module Main where

import qualified Bank

main :: IO ()
main = do
    let bankState  = Bank.emptyState
        bankState' = Bank.wireMoney "A" "B" 12.5 "PLN" bankState
    print bankState'
