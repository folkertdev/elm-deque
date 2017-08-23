module BoundedDeque
    exposing
        ( BoundedDeque
        , empty
        , singleton
        , pushFront
        , pushBack
          --
        , getMaxSize
        , resize
          --
        , isEmpty
        , member
        , length
        , first
        , last
        , popFront
        , popBack
        , takeFront
        , takeBack
        , append
          --
        , map
        , filter
        , foldl
        , foldr
        , partition
          --
        , map2
        , andMap
          --
        , fromList
        , toList
        , fromDeque
        , toDeque
        )

{-| A limited-size deque (double-ended queue).

A deque is a data type for which elements can be efficiently added or removed from either the front or the back.
In this limited-size variant, when the deque is full, an insertion on the front will drop an element at the back, and vice versa.

#Type
@docs BoundedDeque

#Build
@docs empty, singleton, pushFront, pushBack, append

#Lists
@docs fromList, toList

#Deques
@docs fromDeque, toDeque

#Bound
@docs getMaxSize, resize

#Query
@docs isEmpty, member, length, first, last, popFront, popBack, takeFront, takeBack

#Transform
@docs map, filter, foldl, foldr, partition

#Composition
@docs map2, andMap

-}

import Internal
import List


{-| The deque datatype

BoundedDeque equality with `(==)` is unreliable (equivalent deques can have a different distribution of elements between the back
and the front) and should not be used.

-}
type BoundedDeque a
    = BoundedDeque (Internal.AbstractDeque { maxSize : Int } a)


mapAbstract : (Internal.AbstractDeque { maxSize : Int } a -> Internal.AbstractDeque { maxSize : Int } b) -> BoundedDeque a -> BoundedDeque b
mapAbstract f (BoundedDeque abstract) =
    BoundedDeque (f abstract)


unwrap : BoundedDeque a -> Internal.AbstractDeque { maxSize : Int } a
unwrap (BoundedDeque boundedDeque) =
    boundedDeque


{-| Create a bounded deque from an unbounded one. If there is insufficient space, elements are dropped from the back.
-}
fromDeque : Int -> Internal.Deque a -> BoundedDeque a
fromDeque maxSize (Internal.Deque ({ front, rear, sizeF, sizeR } as deque)) =
    let
        delta =
            maxSize - (sizeF + sizeR)
    in
        if delta < 0 then
            -- delta is negative
            Internal.toList deque
                |> fromList maxSize
        else
            BoundedDeque <|
                Internal.rebalance
                    { front = front
                    , rear = rear
                    , sizeF = sizeF
                    , sizeR = sizeR
                    , maxSize = maxSize
                    }


{-| Convert a bounded deque to a normal deque.
-}
toDeque : BoundedDeque a -> Internal.Deque a
toDeque (BoundedDeque { front, rear, sizeF, sizeR }) =
    Internal.Deque <|
        Internal.rebalance
            { front = front
            , rear = rear
            , sizeF = sizeF
            , sizeR = sizeR
            }



-- BUILD


{-| Create an empty deque.
-}
empty : Int -> BoundedDeque a
empty size =
    BoundedDeque (emptyAbstract size)


emptyAbstract : Int -> { front : List b, maxSize : Int, rear : List c, sizeF : number, sizeR : number1 }
emptyAbstract maxSize =
    { sizeF = 0
    , front = []
    , sizeR = 0
    , rear = []
    , maxSize = maxSize
    }


{-| Create a deque with one element.
-}
singleton : Int -> a -> BoundedDeque a
singleton maxSize elem =
    pushFront elem (empty maxSize)


{-| Concatenate two deques into one.

This function is written in pipeline style, so

    firstBoundedDeque
        |> BoundedDeque.append secondBoundedDeque
        |> BoundedDeque.toList

is the same as

    (BoundedDeque.toList firstBoundedDeque)
        |> List.append (BoundedDeque.toList secondBoundedDeque)


The `maxSize` is set to the sum of the two sizes.
-}
append : BoundedDeque a -> BoundedDeque a -> BoundedDeque a
append ((BoundedDeque x) as p) ((BoundedDeque y) as q) =
    if isEmpty p then
        BoundedDeque { y | maxSize = (+) x.maxSize y.maxSize }
    else if isEmpty q then
        BoundedDeque { x | maxSize = (+) x.maxSize y.maxSize }
    else
        BoundedDeque
            { sizeF = x.sizeF + x.sizeR
            , front = x.front ++ List.reverse x.rear
            , sizeR = y.sizeF + y.sizeR
            , rear = List.reverse (y.front ++ List.reverse y.rear)
            , maxSize = (+) x.maxSize y.maxSize
            }


{-| Sets a bound to the number of elements the deque can hold.
a maxSize of Nothing means the deque's size is unbound,
Just a value bounds the deque's size at that value.

If the deque is larger than the bound, items are dropped from the back.

    BoundedDeque.fromList 10 (List.range 0 9)
        |> resize (\_ -> 5)
        -- toList would give [ 0, 1, 2, 3, 4 ]
        |> pushFront 42
        -- toList would give [ 42, 0, 1, 2, 3 ]
        |> pushBack -1
        -- toList would give [ 0, 1, 2, 3, -1 ]
        |> setMaxSize Nothing
        |> pushFront 73
        -- toList would give [ 73, 0, 1, 2, 3 -1 ]

-}
resize : (Int -> Int) -> BoundedDeque a -> BoundedDeque a
resize calculateMaxSize (BoundedDeque deque) =
    let
        newMaxSize =
            calculateMaxSize deque.maxSize
    in
        if deque.sizeF + deque.sizeR <= newMaxSize then
            BoundedDeque { deque | maxSize = newMaxSize }
        else
            toList (BoundedDeque deque)
                |> fromList newMaxSize


{-| Get the maximum number of elements this deque can hold.
-}
getMaxSize : BoundedDeque a -> Int
getMaxSize =
    .maxSize << unwrap


reachedMaxSize : BoundedDeque a -> Bool
reachedMaxSize (BoundedDeque { sizeF, sizeR, maxSize }) =
    sizeF + sizeR == maxSize


{-| Add an element to the front of the deque.

If the deque has reached its maximum capacity, an item is dropped at the back.
-}
pushFront : a -> BoundedDeque a -> BoundedDeque a
pushFront elem ((BoundedDeque { maxSize }) as deque) =
    if maxSize == 0 then
        deque
    else
        let
            (BoundedDeque newBoundedDeque) =
                if reachedMaxSize deque then
                    Tuple.second (popBack deque)
                else
                    deque
        in
            { newBoundedDeque
                | sizeF = newBoundedDeque.sizeF + 1
                , front = elem :: newBoundedDeque.front
            }
                |> BoundedDeque
                |> mapAbstract Internal.rebalance


{-| Add an element to the back of the deque.

If the deque has reached its maximum capacity, an item is dropped at the front.
-}
pushBack : a -> BoundedDeque a -> BoundedDeque a
pushBack elem ((BoundedDeque { maxSize }) as deque) =
    if maxSize == 0 then
        deque
    else
        let
            (BoundedDeque newBoundedDeque) =
                if reachedMaxSize deque then
                    Tuple.second (popFront deque)
                else
                    deque
        in
            { newBoundedDeque
                | sizeR = newBoundedDeque.sizeR + 1
                , rear = elem :: newBoundedDeque.rear
            }
                |> BoundedDeque
                |> mapAbstract Internal.rebalance


{-| Gives Maybe the first element, and the deque without the first element.
If there are no elements, the empty deque is returned.
-}
popFront : BoundedDeque a -> ( Maybe a, BoundedDeque a )
popFront (BoundedDeque deque) =
    deque
        |> Internal.popFront (emptyAbstract deque.maxSize)
        |> Tuple.mapSecond BoundedDeque


{-| Gives Maybe the last element, and the deque without the last element.
If there are no elements, the empty deque is returned.
-}
popBack : BoundedDeque a -> ( Maybe a, BoundedDeque a )
popBack (BoundedDeque deque) =
    deque
        |> Internal.popBack (emptyAbstract deque.maxSize)
        |> Tuple.mapSecond BoundedDeque


{-| Determine if a deque is empty.
-}
isEmpty : BoundedDeque a -> Bool
isEmpty =
    Internal.isEmpty << unwrap


{-| Figure out whether a deque contains a value.
-}
member : a -> BoundedDeque a -> Bool
member elem =
    Internal.member elem << unwrap


{-| Determine the length of a list.
-}
length : BoundedDeque a -> Int
length =
    Internal.length << unwrap


{-| Apply a function to all elements in a deque.

-}
map : (a -> b) -> BoundedDeque a -> BoundedDeque b
map f =
    mapAbstract (Internal.map f)


{-| Like List.map2; apply a function pairwise to two deques.
-}
map2 : (a -> b -> c) -> BoundedDeque a -> BoundedDeque b -> BoundedDeque c
map2 f a b =
    List.map2 f (toList a) (toList b)
        |> fromList (max (getMaxSize a) (getMaxSize b))


{-| Handy function for constructing maps.

to extend to map3 and beyond:

    map3 f a b c =
        map f a
            |> andMap b
            |> andMap c
-}
andMap : BoundedDeque a -> BoundedDeque (a -> b) -> BoundedDeque b
andMap =
    map2 (|>)


{-| Keep an element when it satisfies a predicate.
-}
filter : (a -> Bool) -> BoundedDeque a -> BoundedDeque a
filter p =
    mapAbstract (Internal.filter p)


{-| Fold over the deque from left to right (highest priority to lowest priority).
-}
foldl : (a -> b -> b) -> b -> BoundedDeque a -> b
foldl f initial =
    Internal.foldl f initial << unwrap


{-| Fold over the deque from right to left (lowest priority to highest priority).
-}
foldr : (a -> b -> b) -> b -> BoundedDeque a -> b
foldr f initial =
    Internal.foldr f initial << unwrap


{-| Partition a deque according to a predicate. The first deque contains
all elements that satisfy the predicate, and the second contains the rest.
-}
partition : (a -> Bool) -> BoundedDeque a -> ( BoundedDeque a, BoundedDeque a )
partition p (BoundedDeque deque) =
    let
        ( l1, r1 ) =
            List.partition p deque.front

        ( l2, r2 ) =
            List.partition p deque.rear
    in
        ( fromList deque.maxSize (l1 ++ l2), fromList deque.maxSize (r1 ++ r2) )


{-| Extract the first element of a deque
-}
first : BoundedDeque a -> Maybe a
first =
    Internal.first << unwrap


{-| Extract the last element of a deque.
-}
last : BoundedDeque a -> Maybe a
last =
    Internal.last << unwrap


{-| Take the first `n` members of a deque.

    BoundedDeque.fromList [2..10]
        |> BoundedDeque.takeBack 3
        -- == [ 2, 3, 4 ]
-}
takeFront : Int -> BoundedDeque a -> List a
takeFront i =
    Internal.takeFront i << unwrap


{-| Take the last `n` members of a deque.

    BoundedDeque.fromList [2..10]
        |> BoundedDeque.takeBack 3
        -- == [ 10, 9, 8 ]
-}
takeBack : Int -> BoundedDeque a -> List a
takeBack i =
    Internal.takeBack i << unwrap


{-| Convert a deque to a list.
-}
toList : BoundedDeque a -> List a
toList =
    Internal.toList << unwrap


{-| Create a bounded deque from a maximum size and a list.
-}
fromList : Int -> List a -> BoundedDeque a
fromList maxSize elements =
    List.take maxSize elements
        |> Internal.fromList (emptyAbstract maxSize)
        |> BoundedDeque
