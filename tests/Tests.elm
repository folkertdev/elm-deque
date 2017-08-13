module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import Deque exposing (Deque, fromList, toList, pushFront, pushBack)
import Tuple exposing (second)


deque : Fuzz.Fuzzer a -> Fuzz.Fuzzer (Deque a)
deque =
    Fuzz.map fromList << list


construction : Test
construction =
    describe "Construction"
        [ fuzz (deque int) "push << pop == identity; front" <|
            \ints ->
                (Deque.pushFront 1 >> Deque.popFront >> second >> toList) ints
                    |> Expect.equal (toList ints)
        , fuzz (deque int) "push << pop == identity; back" <|
            \ints ->
                (Deque.pushBack 1 >> Deque.popBack >> second >> toList) ints
                    |> Expect.equal (toList ints)
        , fuzz (list int) "fromList << pushLeft v << toList == v ::" <|
            \ints ->
                (fromList >> pushFront 42 >> toList) ints
                    |> Expect.equal (42 :: ints)
        , fuzz (list int) "fromList << pushRight v << toList == (\\l -> l ++ [v]" <|
            \ints ->
                (fromList >> pushBack 42 >> toList) ints
                    |> Expect.equal (ints ++ [ 42 ])
        , fuzz (tuple ( deque int, deque int )) "toList (append a b) == (toList a) ++ (toList b)" <|
            \( firstDeque, secondDeque ) ->
                let
                    given =
                        firstDeque
                            |> Deque.append secondDeque
                            |> Deque.toList

                    expected =
                        (Deque.toList firstDeque)
                            |> List.append (Deque.toList secondDeque)
                in
                    given
                        |> Expect.equal expected
        ]


folds : Test
folds =
    describe "folds"
        [ fuzz (deque string) "foldl f d == foldl f d << toList" <|
            \strings ->
                let
                    given =
                        strings
                            |> Deque.foldl (++) ""

                    expected =
                        strings
                            |> Deque.toList
                            |> List.foldl (++) ""
                in
                    given
                        |> Expect.equal expected
        , fuzz (deque string) "foldr f d == foldr f d << toList" <|
            \strings ->
                let
                    given =
                        strings
                            |> Deque.foldr (++) ""

                    expected =
                        strings
                            |> Deque.toList
                            |> List.foldr (++) ""
                in
                    given
                        |> Expect.equal expected
        ]


lists : Test
lists =
    describe "FromList and ToList"
        [ fuzz (list int) "fromList << toList == identity" <|
            \ints ->
                toList (fromList ints)
                    |> Expect.equal ints
        , fuzz (deque int) "toList << fromList == identity" <|
            \ints ->
                fromList (toList ints)
                    |> Expect.equal ints
        ]


all : Test
all =
    describe "Sample Test Suite"
        [ lists
        , construction
        ]
