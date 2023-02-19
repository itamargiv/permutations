module Permutations exposing (ofList)

{-|
@docs ofList
-}


import List

{-| Type alias to represent permutations, which are essentially a list of lists
-}
type alias Permutations a = List (List a)

{-| Type alias to represent a pair of values to interleave into a list
-}
type alias Interleaver a = (a , List a)

{-| Return a list of all possible permutations of a list.

    fromList [0, 1] == [[0, 1], [1, 0]]
-}
ofList : List a -> List (List a)
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
