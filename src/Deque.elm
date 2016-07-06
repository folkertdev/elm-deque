module Deque
    exposing
        ( Deque
        , empty
        , singleton
        , pushFront
        , pushBack
          --
        , isEmpty
        , member
        , first
        , last
        , popFront
        , popBack
        , takeFront
        , takeBack
          --
        , map
        , filter
        , foldl
        , foldr
        , partition
          --
        , map2
        , andMap
        , (<*>)
        , andThen
        , join
          --
        , fromList
        , toList
        )

{-| A Deque (double-ended queue) in Elm.

A deque is a data type for which elements can be efficiently added or removed from either the front or the back.

Internally, this is a head-tail linked list, modelled after [deque in Haskell](https://hackage.haskell.org/package/deque-0.1.12/docs/Data-Dequeue.html) which
in turn is based on Chris Okasaki's Purely Functional Data Structures. A head-tail linked list is based on two lists: one for the head and one for the tail.
This means that pop and push on either side are operations on the front portion of an elm list, which is very efficient (`O(n)`).

The deque rebalances (moves elements from the front to the rear or vice versa) when either one
is 4 times as large as the other. This is a costly operation and therefore used as little as possible.


#Type and Constructors
@docs Deque

#Build
@docs empty, singleton, pushFront, pushBack

#Query
@docs isEmpty, member, first, last, popFront, popBack, takeFront, takeBack

#Transform
@docs map, filter, foldl, foldr, partition

#Lists
@docs fromList, toList

#Abstract Nonsense

*These terms and functions come from category theory and the programming language Haskell. In elm, they are of generally of little importance, but may come
in handy.*

Primitives to use a `Deque` as an applicative and a monad,
respecting the [applicative](https://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Applicative.html) and [monad](https://wiki.haskell.org/Monad_laws) laws.
Instances are based on (and as such, identical to) the standard implementations for list
and are verified with property-based testing (see [the tests](https://github.com/folkertdev/elm-deque/tree/master/test/Test.elm)).

@docs map2, andMap, (<*>), andThen, join

-}

import Debug
import List


{-| The deque datatype

Deque equality with `(==)` is unreliable (equivalent deques can have a different distribution of elements between the back
and the front) and should not be used.

Internally deque is modelled as:

    type Deque a
        = Deque Int (List a) Int (List a)
-}
type Deque a
    = Deque Int (List a) Int (List a)


{-| Create an empty deque.
-}
empty : Deque a
empty =
    Deque 0 [] 0 []


{-| Create a deque with one element.
-}
singleton : a -> Deque a
singleton elem =
    Deque 1 [ elem ] 0 []


{-| Add an element to the front of the deque.
-}
pushFront : a -> Deque a -> Deque a
pushFront elem (Deque sizeF front sizeR rear) =
    rebalance <| Deque (sizeF + 1) (elem :: front) sizeR rear


{-| Add an element to the back of the deque.
-}
pushBack : a -> Deque a -> Deque a
pushBack elem (Deque sizeF front sizeR rear) =
    rebalance <| Deque sizeF front (sizeR + 1) (elem :: rear)


{-| Gives Maybe the first element, and the deque without the first element.
If there are no elements, the empty deque is returned.
-}
popFront : Deque a -> ( Maybe a, Deque a )
popFront deque =
    case deque of
        Deque _ [] _ [] ->
            ( Nothing, empty )

        Deque _ [] _ [ x ] ->
            ( Just x, empty )

        Deque _ [] _ _ ->
            Debug.crash "Deque is too far unbalanced"

        Deque sizeF (f :: fs) sizeR rear ->
            ( Just f, rebalance <| Deque (sizeF - 1) fs sizeR rear )


{-| Gives Maybe the last element, and the deque without the last element.
If there are no elements, the empty deque is returned.
-}
popBack : Deque a -> ( Maybe a, Deque a )
popBack deque =
    case deque of
        Deque _ [] _ [] ->
            ( Nothing, empty )

        Deque _ [ x ] _ [] ->
            ( Just x, empty )

        Deque _ _ _ [] ->
            Debug.crash "Deque is too far unbalanced"

        Deque sizeF front sizeR (r :: rs) ->
            ( Just r, rebalance <| Deque sizeF front (sizeR - 1) rs )


{-| Determine if a deque is empty.
-}
isEmpty : Deque a -> Bool
isEmpty deque =
    case deque of
        Deque 0 [] 0 [] ->
            True

        _ ->
            False


{-| Figure out whether a deque contains a value.
-}
member : a -> Deque a -> Bool
member elem (Deque _ front _ rear) =
    (List.member elem front) || (List.member elem rear)


{-| Determine the length of a list.
-}
length : Deque a -> Int
length (Deque sizeF _ sizeR _) =
    sizeF + sizeR


{-| Apply a function to all elements in a deque.
-}
map : (a -> b) -> Deque a -> Deque b
map f (Deque sizeF front sizeR rear) =
    Deque sizeF (List.map f front) sizeR (List.map f rear)


{-| Apply a function of two arguments to the elements of two deques. The
result has the length of the smallest deque (just like lists).
-}
map2 : (a -> b -> c) -> Deque a -> Deque b -> Deque c
map2 f a b =
    fromList <| List.map2 f (toList a) (toList b)


{-| Allows for building up a deque from multiple deques
Every function in the first deque is mapped to all elements of the second one.
The result is then concatenated.

    fromList [ abs, (\x -> x + 2) ] `andMap` fromList [ -2, 4 ]
        == fromList [ 2, 4, 0, 6 ]
-}
andMap : Deque (a -> b) -> Deque a -> Deque b
andMap fs xs =
    -- elm-format butchers this expression if it is not on one line
    fs `andThen` \f -> xs `andThen` \x -> singleton (f x)


{-| Infix version of andMap: left associative with precedence level 4.
-}
(<*>) : Deque (a -> b) -> Deque a -> Deque b
(<*>) =
    andMap
infixl 4 <*>


(<$>) =
    -- infix version of map/fmap. Only for internal use
    map
infixl 4 <$>


{-| Deque equivalent of List.concat.
-}
join : Deque (Deque a) -> Deque a
join deque =
    let
        list =
            map toList deque
                |> toList
                |> List.concat
                |> fromList
    in
        list


{-| Map a given function onto a deque and flatten the resulting deque.
Works similar to concatMap on lists.
-}
andThen : Deque a -> (a -> Deque b) -> Deque b
andThen deque f =
    map f deque
        |> join


{-| Keep an element when it satisfies a predicate.
-}
filter : (a -> Bool) -> Deque a -> Deque a
filter p (Deque _ front _ rear) =
    let
        newFront =
            List.filter p front

        newRear =
            List.filter p rear
    in
        Deque (List.length newFront) newFront (List.length newRear) newRear
            |> rebalance


{-| Fold over the deque from left to right (highest priority to lowest priority).
-}
foldl : (a -> b -> b) -> b -> Deque a -> b
foldl f initial (Deque sizeF front sizeR rear) =
    List.foldl f initial (front ++ List.reverse rear)


{-| Fold over the deque from right to left (lowest priority to highest priority).
-}
foldr : (a -> b -> b) -> b -> Deque a -> b
foldr f initial (Deque sizeF front sizeR rear) =
    List.foldr f initial (front ++ List.reverse rear)


{-| Partition a deque according to a predicate. The first deque contains
all elements that satisfy the predicate, and the second contains the rest.
-}
partition : (a -> Bool) -> Deque a -> ( Deque a, Deque a )
partition p (Deque _ front _ rear) =
    let
        ( l1, r1 ) =
            List.partition p front

        ( l2, r2 ) =
            List.partition p rear
    in
        ( fromList (l1 ++ l2), fromList (r1 ++ r2) )


{-| Extract the first element of a deque
-}
first : Deque a -> Maybe a
first deque =
    case deque of
        Deque _ [] _ [ x ] ->
            Just x

        Deque _ front _ _ ->
            List.head front


{-| Extract the last element of a deque.
-}
last : Deque a -> Maybe a
last deque =
    case deque of
        Deque _ [ x ] _ [] ->
            Just x

        Deque _ _ _ rear ->
            List.head rear


{-| Take the first `n` members of a deque.
-}
takeFront : Int -> Deque a -> List a
takeFront i (Deque sizeF front _ rear) =
    List.take i front ++ List.take (i - sizeF) (List.reverse rear)


{-| Take the last `n` members of a deque.
-}
takeBack : Int -> Deque a -> List a
takeBack i (Deque _ front sizeR rear) =
    List.take i rear ++ List.take (i - sizeR) (List.reverse front)


{-| Rebalance the deque. This is an internal function and should not normally be
called from the outside.
-}
rebalance : Deque a -> Deque a
rebalance ((Deque sizeF front sizeR rear) as deque) =
    let
        -- the maximum number of times that one half
        -- of the deque may be longer than the other
        balanceConstant : Int
        balanceConstant =
            4

        size1 =
            (sizeF + sizeR) // 2

        size2 =
            (sizeF + sizeR) - size1
    in
        if sizeF + sizeR < 2 then
            deque
        else if sizeF > balanceConstant * sizeR + 1 then
            let
                newFront =
                    List.take size1 front

                newRear =
                    rear ++ List.reverse (List.drop size1 front)
            in
                Deque size1 newFront size2 newRear
        else if sizeR > balanceConstant * sizeF + 1 then
            let
                newFront =
                    front ++ List.reverse (List.drop size1 rear)

                newRear =
                    List.take size1 rear
            in
                Deque size2 newFront size1 newRear
        else
            deque


{-| Convert a deque to a list.
-}
toList : Deque a -> List a
toList (Deque _ front _ rear) =
    front ++ List.reverse rear


{-| Create a deque from a list.
-}
fromList : List a -> Deque a
fromList list =
    rebalance <| Deque (List.length list) list 0 []
