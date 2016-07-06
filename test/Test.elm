module Main exposing (..)

import Dequeue exposing (..)
import Check exposing (Claim, Evidence, suite, claim, that, is, for, quickCheck)
import Check.Producer as Producer exposing (list, int)
import Check.Test
import ElmTest
import Debug


dequeue =
    Producer.map fromList << list


functor : Claim
functor =
    suite "Check the functor laws"
        [ claim "Dequeue.map identity == identity"
            `that` (Dequeue.map identity)
            `is` identity
            `for` dequeue int
        , claim "Dequeue.filter (always True) == identity"
            `that` (Dequeue.filter (\_ -> True))
            `is` identity
            `for` dequeue int
        ]


monad : Claim
monad =
    let
        -- f : a -> Dequeue b
        f a =
            pushFront a <| fromList [ 42 ]

        -- f : a -> Dequeue b
        g a =
            pushBack a <| fromList [ -42 ]
    in
        suite "Check the monad laws"
            [ claim "left identity"
                `that` (toList << (\v -> (singleton v) `andThen` f))
                `is` (toList << (\v -> f v))
                `for` int
            , claim "right identity"
                `that` (toList << (\m -> m `andThen` singleton))
                `is` (toList << identity)
                `for` dequeue int
            , claim "associativity"
                `that` (toList << (\m -> (m `andThen` f) `andThen` g))
                `is` (toList << (\m -> m `andThen` (\x -> f x `andThen` g)))
                `for` dequeue int
            ]


applicative : Claim
applicative =
    let
        toFunc v =
            map (+) v
    in
        suite "Check the applicative laws"
            [ claim "map2"
                `that` (toList << (\v -> map2 (+) v v))
                `is` (\v -> List.map2 (+) (toList v) (toList v))
                `for` dequeue int
            , claim "identity"
                `that` (toList << (\v -> (singleton identity) `andMap` v))
                `is` (toList)
                `for` dequeue int
            , claim "homomorphism"
                `that` (toList << (\v -> (singleton abs) `andMap` (singleton v)))
                `is` (toList << (\v -> singleton (abs v)))
                `for` int
            , claim "interchange"
                `that` (toList << (\v -> (toFunc v) `andMap` (singleton 2)))
                `is` (toList << (\v -> singleton (\a -> a <| 2) `andMap` (toFunc v)))
                `for` dequeue int
            , claim "composition"
                `that` (toList
                            << (\a ->
                                    let
                                        u =
                                            map (+) (singleton 2)

                                        v =
                                            map (-) a

                                        w =
                                            singleton 2
                                    in
                                        u <*> (v <*> w)
                               )
                       )
                `is` (toList
                        << (\a ->
                                let
                                    u =
                                        map (+) (singleton 2)

                                    v =
                                        map (-) a

                                    w =
                                        singleton 2
                                in
                                    singleton (<<) <*> u <*> v <*> w
                           )
                     )
                `for` dequeue int
            ]


construction : Claim
construction =
    suite "Construction"
        [ claim "push << pop == identity; front"
            `that` (Dequeue.pushFront 1 >> Dequeue.popFront >> Maybe.map (snd >> toList))
            `is` (Just << toList)
            `for` dequeue int
        , claim "push << pop == identity; back"
            `that` (Dequeue.pushBack 1 >> Dequeue.popBack >> Maybe.map (snd >> toList))
            `is` (Just << toList)
            `for` dequeue int
        , claim "fromList << pushLeft v << toList == v ::"
            `that` (fromList >> pushFront 42 >> toList)
            `is` (\l -> 42 :: l)
            `for` list int
        , claim "fromList << pushRight v << toList == (\\l -> l ++ [v]"
            `that` (fromList >> pushBack 42 >> toList)
            `is` (\l -> l ++ [ 42 ])
            `for` list int
        , claim "fromList >> popLeft v >> toList == tail "
            `that` (fromList >> popFront >> Maybe.map (snd >> toList))
            `is` List.tail
            `for` list int
        , claim "fromList >> popBack >> toList == tail "
            `that` (fromList >> popBack >> Maybe.map (snd >> toList))
            `is` (\l -> init l)
            `for` list int
        ]


init l =
    case List.length l of
        0 ->
            Nothing

        1 ->
            Just []

        _ as len ->
            Just (List.take (len - 1) l)


lists : Claim
lists =
    suite "FromList and ToList"
        [ claim "fromList << toList == identity"
            `that` (toList << fromList)
            `is` identity
            `for` list int
        , claim "toList << fromList == identity"
            `that` (fromList << toList)
            `is` identity
            `for` dequeue int
        ]


claims : Claim
claims =
    suite "Claims about Dequeue"
        [ functor
        , applicative
        , monad
        , lists
        , construction
        ]


evidence : Evidence
evidence =
    quickCheck claims


main =
    ElmTest.runSuite (Check.Test.evidenceToTest evidence)
