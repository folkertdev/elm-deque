module Deque exposing
    ( Deque
    , empty, singleton, pushFront, pushBack, append
    , fromList, toList
    , isEmpty, member, length, first, last, popFront, popBack, takeFront, takeBack
    , map, filter, foldl, foldr, partition
    , map2, andMap
    )

{-| A Deque (double-ended queue) in Elm.

A deque is a data type for which elements can be efficiently added or removed from either the front or the back.

Internally, this is a head-tail linked list, modeled after this [deque in Haskell](https://hackage.haskell.org/package/dequeue-0.1.12/docs/Data-Dequeue.html) which
in turn is based on Chris Okasaki's Purely Functional Data Structures. A head-tail linked list is based on two lists: one for the head and one for the tail.
This means that pop and push on either side are operations on the front portion of an elm list, which is very efficient (`O(1)`).

The deque rebalances (moves elements from the front to the rear or vice versa) when either one
is 4 times as large as the other. This is a costly operation and therefore used as little as possible.

For a deque with a limited size, see [BoundedDeque](#BoundedDeque).


## Type

@docs Deque


## Build

@docs empty, singleton, pushFront, pushBack, append


## Lists

@docs fromList, toList


## Query

@docs isEmpty, member, length, first, last, popFront, popBack, takeFront, takeBack


## Transform

@docs map, filter, foldl, foldr, partition


## Composition

@docs map2, andMap

-}

import Internal
import List


{-| The deque datatype

Deque equality with `(==)` is unreliable (equivalent deques can have a different distribution of elements between the back
and the front) and should not be used.

-}
type Deque a
    = Deque (Internal.Deque a)


mapAbstract : (Internal.Deque a -> Internal.Deque b) -> Deque a -> Deque b
mapAbstract f (Deque abstract) =
    Deque (f abstract)


unwrap : Deque a -> Internal.Deque a
unwrap (Deque boundedDeque) =
    boundedDeque



-- BUILD


{-| Create an empty deque.
-}
empty : Deque a
empty =
    Deque Internal.empty


{-| Create a deque with one element.
-}
singleton : a -> Deque a
singleton elem =
    pushFront elem empty


{-| Concatenate two deques into one.

This function is written in pipeline style, so

    firstDeque
        |> Deque.append secondDeque
        |> Deque.toList

is the same as

    Deque.toList firstDeque
        |> List.append (Deque.toList secondDeque)

-}
append : Deque a -> Deque a -> Deque a
append ((Deque x) as p) ((Deque y) as q) =
    if isEmpty p then
        q

    else if isEmpty q then
        p

    else
        Deque
            { sizeF = x.sizeF + x.sizeR
            , front = x.front ++ List.reverse x.rear
            , sizeR = y.sizeF + y.sizeR
            , rear = List.reverse (y.front ++ List.reverse y.rear)
            }


{-| Add an element to the front of the deque.
-}
pushFront : a -> Deque a -> Deque a
pushFront elem (Deque deque) =
    { sizeF = deque.sizeF + 1
    , front = elem :: deque.front
    , sizeR = deque.sizeR
    , rear = deque.rear
    }
        |> Deque
        |> mapAbstract Internal.rebalance


{-| Add an element to the back of the deque.
-}
pushBack : a -> Deque a -> Deque a
pushBack elem (Deque deque) =
    { sizeR = deque.sizeR + 1
    , rear = elem :: deque.rear
    , sizeF = deque.sizeF
    , front = deque.front
    }
        |> Deque
        |> mapAbstract Internal.rebalance


{-| Gives Maybe the first element, and the deque without the first element.
If there are no elements, the empty deque is returned.
-}
popFront : Deque a -> ( Maybe a, Deque a )
popFront =
    Tuple.mapSecond Deque << Internal.popFront << unwrap


{-| Gives Maybe the last element, and the deque without the last element.
If there are no elements, the empty deque is returned.
-}
popBack : Deque a -> ( Maybe a, Deque a )
popBack =
    Tuple.mapSecond Deque << Internal.popBack << unwrap


{-| Determine if a deque is empty.
-}
isEmpty : Deque a -> Bool
isEmpty =
    Internal.isEmpty << unwrap


{-| Figure out whether a deque contains a value.
-}
member : a -> Deque a -> Bool
member elem =
    Internal.member elem << unwrap


{-| Determine the length of a list.
-}
length : Deque a -> Int
length =
    Internal.length << unwrap


{-| Apply a function to all elements in a deque.
-}
map : (a -> b) -> Deque a -> Deque b
map f =
    mapAbstract (Internal.map f)


{-| Like List.map2; apply a function pairwise to two deques.
-}
map2 : (a -> b -> c) -> Deque a -> Deque b -> Deque c
map2 f a b =
    List.map2 f (toList a) (toList b)
        |> fromList


{-| Handy function for constructing maps.

to extend to map3 and beyond:

    map3 f a b c =
        map f a
            |> andMap b
            |> andMap c

-}
andMap : Deque a -> Deque (a -> b) -> Deque b
andMap =
    map2 (|>)


{-| Keep an element when it satisfies a predicate.
-}
filter : (a -> Bool) -> Deque a -> Deque a
filter p =
    mapAbstract (Internal.filter p)


{-| Fold over the deque from left to right (highest priority to lowest priority).
-}
foldl : (a -> b -> b) -> b -> Deque a -> b
foldl f initial =
    Internal.foldl f initial << unwrap


{-| Fold over the deque from right to left (lowest priority to highest priority).
-}
foldr : (a -> b -> b) -> b -> Deque a -> b
foldr f initial =
    Internal.foldr f initial << unwrap


{-| Partition a deque according to a predicate. The first deque contains
all elements that satisfy the predicate, and the second contains the rest.
-}
partition : (a -> Bool) -> Deque a -> ( Deque a, Deque a )
partition p (Deque deque) =
    let
        ( l1, r1 ) =
            List.partition p deque.front

        ( l2, r2 ) =
            List.partition p deque.rear
    in
    ( fromList (l1 ++ l2), fromList (r1 ++ r2) )


{-| Extract the first element of a deque
-}
first : Deque a -> Maybe a
first =
    Internal.first << unwrap


{-| Extract the last element of a deque.
-}
last : Deque a -> Maybe a
last =
    Internal.last << unwrap


{-| Take the first `n` members of a deque.

    Deque.fromList [2..10]
        |> Deque.takeBack 3
        -- == [ 2, 3, 4 ]

-}
takeFront : Int -> Deque a -> List a
takeFront i =
    Internal.takeFront i << unwrap


{-| Take the last `n` members of a deque.

    Deque.fromList [2..10]
        |> Deque.takeBack 3
        -- == [ 10, 9, 8 ]

-}
takeBack : Int -> Deque a -> List a
takeBack i =
    Internal.takeBack i << unwrap


{-| Convert a deque to a list.
-}
toList : Deque a -> List a
toList =
    Internal.toList << unwrap


{-| Create a deque from a list.
-}
fromList : List a -> Deque a
fromList =
    Deque << Internal.fromList
