module SCPropTest where

import Data.List (sort)

scprop_sort_reverse :: [Int] -> Bool
scprop_sort_reverse list = sort list == sort (reverse list)
