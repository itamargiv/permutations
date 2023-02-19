module Permutations exposing (ofList)

{-| This library allows for an easy way to quickly create permutations of lists.
it is heavily based on the permutations function as written for Haskell in the 
[Data.List module](https://hackage.haskell.org/package/base-4.17.0.0/docs/src/Data.OldList.html#permutations).

# Permutations
@docs ofList
-}


import List

{-| Type alias to represent permutations, which are essentially a list of lists
-}
type alias Permutations a = List (List a)
type alias Interleaver a = (a , List a)



{-| Return a list of all possible permutations of a list.

    fromList [0, 1] == [[0, 1], [1, 0]]
-}
ofList : List a -> Permutations a
ofList list = 
    let 
        permute interim base =
            case interim of 
                [] -> []
                x :: xs -> List.foldr 
                    (interleave (x, xs)) 
                    (permute xs (x :: base)) 
                    (ofList base)
    in list :: permute list []

{-| Interleave a value between all values of a list and a base and attach it to 
a provided initial value.

    interleave (2, []) [0, 1] [[2, 1, 0], [1, 2, 0]] 
        == [[2, 0, 1], [0, 2, 1], [2, 1, 0], [1, 2, 0]]
-}
interleave : Interleaver a -> List a -> Permutations a -> Permutations a
interleave (n, base) list init = 
    let
        interim transform current acc =
            case current of
                [] -> (base, acc)
                x :: xs ->
                    let (iBase, iAcc) = interim (transform >> (::) x) xs acc
                    in (x :: iBase, (transform (n :: x :: iBase) :: iAcc)) 

        (_, result) = interim (identity) list init
    in result
