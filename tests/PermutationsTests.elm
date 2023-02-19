module PermutationsTests exposing (..)

import Expect
import Test exposing (..)

import Permutations

suite : Test
suite =
    describe "The Permutation module"
        [ describe "Permutation.ofList"
            [ test "Returns a list of all permutations" <|
                \_ -> 
                    let
                        given = [0, 1]
                        expected = [[0, 1], [1, 0]]
                    in 
                        Expect.equal (Permutations.ofList given) expected
            , test "Returns a list of more permutations" <|
                \_ -> 
                    let
                        given = [0, 1, 2]
                        expected = 
                            [ [0, 1, 2]
                            , [1, 0, 2]
                            , [2, 1, 0]
                            , [1, 2, 0]
                            , [2, 0, 1]
                            , [0, 2, 1]
                            ]
                    in 
                        Expect.equal (Permutations.ofList given) expected
            , test "Returns empty list singleton for empty list" <|
                \_ -> Expect.equal (Permutations.ofList []) [[]]
            , test "Returns singleton for singleton" <|
                \_ -> Expect.equal (Permutations.ofList [0]) [[0]]
            ]    
        ]
