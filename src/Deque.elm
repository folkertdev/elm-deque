module Deque
    exposing
        ( Deque
        , empty
        , singleton
        , pushFront
        , pushBack
          --
        , getMaxSize
        , setMaxSize
          --
        , isEmpty
        , member
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
        , fromList
        , toList
        )

{-| A Deque (double-ended queue) in Elm.

A deque is a data type for which elements can be efficiently added or removed from either the front or the back.

Internally, this is a head-tail linked list, modeled after this [deque in Haskell](https://hackage.haskell.org/package/dequeue-0.1.12/docs/Data-Dequeue.html) which
in turn is based on Chris Okasaki's Purely Functional Data Structures. A head-tail linked list is based on two lists: one for the head and one for the tail.
This means that pop and push on either side are operations on the front portion of an elm list, which is very efficient (`O(1)`).

The deque rebalances (moves elements from the front to the rear or vice versa) when either one
is 4 times as large as the other. This is a costly operation and therefore used as little as possible.

It is possible to set a maximum number of elements for the deque. The default is an unlimited
size. When an item is pushed onto a full deque, an item is popped (and discarded) at the other end.


#Type
@docs Deque

#Build
@docs empty, singleton, pushFront, pushBack, append

#Lists
@docs fromList, toList

#Bound
@docs getMaxSize, setMaxSize

#Query
@docs isEmpty, member, first, last, popFront, popBack, takeFront, takeBack

#Transform

Simple transform functions. To use more complex functions, like `map2` or `concat`, just
convert the deque to a list, apply the operation and convert back.

@docs map, filter, foldl, foldr, partition


-}

import List


{-| The deque datatype

Deque equality with `(==)` is unreliable (equivalent deques can have a different distribution of elements between the back
and the front) and should not be used.

-}
type Deque a
    = Deque (Internal a)


type alias Internal a =
    { sizeF : Int
    , front : List a
    , sizeR : Int
    , rear : List a
    , maxSize : Maybe Int
    }


mapInternal : (Internal a -> Internal b) -> Deque a -> Deque b
mapInternal f (Deque internal) =
    Deque (f internal)


unwrap : Deque a -> Internal a
unwrap (Deque internal) =
    internal



-- BUILD


{-| Create an empty deque.
-}
empty : Deque a
empty =
    Deque
        { sizeF = 0
        , front = []
        , sizeR = 0
        , rear = []
        , maxSize = Nothing
        }


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

    (Deque.toList firstDeque)
        |> List.append (Deque.toList secondDeque)


The `maxSize` is set to the sum of the two sizes; if either `maxSize` is Nothing, the result `maxSize` is Nothing.
-}
append : Deque a -> Deque a -> Deque a
append ((Deque x) as p) ((Deque y) as q) =
    if isEmpty p then
        Deque { y | maxSize = Maybe.map2 (+) x.maxSize y.maxSize }
    else if isEmpty q then
        Deque { x | maxSize = Maybe.map2 (+) x.maxSize y.maxSize }
    else
        Deque
            { sizeF = x.sizeF + x.sizeR
            , front = x.front ++ List.reverse x.rear
            , sizeR = y.sizeF + y.sizeR
            , rear = List.reverse (y.front ++ List.reverse y.rear)
            , maxSize = Maybe.map2 (+) x.maxSize y.maxSize
            }


{-| Sets a bound to the number of elements the deque can hold.
a maxSize of Nothing means the deque's size is unbound,
Just a value bounds the deque's size at that value.

If the deque is larger than the bound, items are dropped from the back.

    Deque.fromList [0..9]
        |> setMaxSize (Just 5)
        -- toList would give [ 0, 1, 2, 3, 4 ]
        |> pushFront 42
        -- toList would give [ 42, 0, 1, 2, 3 ]
        |> pushBack -1
        -- toList would give [ 0, 1, 2, 3, -1 ]
        |> setMaxSize Nothing
        |> pushFront 73
        -- toList would give [ 73, 0, 1, 2, 3 -1 ]

-}
setMaxSize : Maybe Int -> Deque a -> Deque a
setMaxSize mbound =
    case mbound of
        Nothing ->
            mapInternal (\deque -> { deque | maxSize = mbound })

        Just bound ->
            fromList << takeFront bound << mapInternal (\deque -> { deque | maxSize = mbound })


{-| Get the maximum number of elements this deque can hold. A value of Nothing
means the deque can hold an unlimited number of items (which is the default).
-}
getMaxSize : Deque a -> Maybe Int
getMaxSize =
    .maxSize << unwrap


reachedMaxSize : Deque a -> Bool
reachedMaxSize (Deque { sizeF, sizeR, maxSize }) =
    Just (sizeF + sizeR) == maxSize


{-| Add an element to the front of the deque.
-}
pushFront : a -> Deque a -> Deque a
pushFront elem ((Deque { maxSize }) as deque) =
    let
        (Deque newDeque) =
            if reachedMaxSize deque then
                Tuple.second (popBack deque)
            else
                deque
    in
        { newDeque
            | sizeF = newDeque.sizeF + 1
            , front = elem :: newDeque.front
        }
            |> Deque
            |> rebalance


{-| Add an element to the back of the deque.
-}
pushBack : a -> Deque a -> Deque a
pushBack elem deque =
    let
        (Deque newDeque) =
            if reachedMaxSize deque then
                Tuple.second (popFront deque)
            else
                deque
    in
        { newDeque
            | sizeR = newDeque.sizeR + 1
            , rear = elem :: newDeque.rear
        }
            |> Deque
            |> rebalance


{-| Gives Maybe the first element, and the deque without the first element.
If there are no elements, the empty deque is returned.
-}
popFront : Deque a -> ( Maybe a, Deque a )
popFront ((Deque { front, rear }) as deque) =
    case ( front, rear ) of
        ( [], [] ) ->
            ( Nothing, empty )

        ( [], [ x ] ) ->
            ( Just x, empty )

        ( [], _ ) ->
            Debug.crash "Deque is too far unbalanced"

        ( f :: fs, _ ) ->
            ( Just f
            , mapInternal (\deque -> { deque | sizeF = deque.sizeF - 1, front = fs }) deque
                |> rebalance
            )


{-| Gives Maybe the last element, and the deque without the last element.
If there are no elements, the empty deque is returned.
-}
popBack : Deque a -> ( Maybe a, Deque a )
popBack ((Deque { front, rear }) as deque) =
    case ( front, rear ) of
        ( [], [] ) ->
            ( Nothing, empty )

        ( [ x ], [] ) ->
            ( Just x, empty )

        ( _, [] ) ->
            Debug.crash "Deque is too far unbalanced"

        ( _, r :: rs ) ->
            ( Just r
            , mapInternal (\deque -> { deque | sizeR = deque.sizeR - 1, rear = rs }) deque
                |> rebalance
            )


{-| Determine if a deque is empty.
-}
isEmpty : Deque a -> Bool
isEmpty deque =
    length deque == 0


{-| Figure out whether a deque contains a value.
-}
member : a -> Deque a -> Bool
member elem (Deque deque) =
    List.member elem deque.front || List.member elem deque.rear


{-| Determine the length of a list.
-}
length : Deque a -> Int
length (Deque deque) =
    deque.sizeF + deque.sizeR


{-| Apply a function to all elements in a deque.

-}
map : (a -> b) -> Deque a -> Deque b
map f (Deque deque) =
    Deque
        { deque
            | front = List.map f deque.front
            , rear = List.map f deque.rear
        }


{-| Keep an element when it satisfies a predicate.
-}
filter : (a -> Bool) -> Deque a -> Deque a
filter p (Deque deque) =
    let
        newFront =
            List.filter p deque.front

        newRear =
            List.filter p deque.rear
    in
        { deque
            | sizeF = List.length newFront
            , front = newFront
            , sizeR = List.length newRear
            , rear = newRear
        }
            |> Deque
            |> rebalance


{-| Fold over the deque from left to right (highest priority to lowest priority).
-}
foldl : (a -> b -> b) -> b -> Deque a -> b
foldl f initial (Deque deque) =
    List.foldl f initial deque.front
        |> (\initial_ -> List.foldr f initial_ deque.rear)


{-| Fold over the deque from right to left (lowest priority to highest priority).
-}
foldr : (a -> b -> b) -> b -> Deque a -> b
foldr f initial (Deque deque) =
    List.foldl f initial deque.rear
        |> (\initial_ -> List.foldr f initial_ deque.front)


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
first (Deque deque) =
    case ( deque.front, deque.rear ) of
        ( [], [ x ] ) ->
            Just x

        _ ->
            List.head deque.front


{-| Extract the last element of a deque.
-}
last : Deque a -> Maybe a
last (Deque deque) =
    case ( deque.front, deque.rear ) of
        ( [ x ], [] ) ->
            Just x

        _ ->
            List.head deque.rear


{-| Take the first `n` members of a deque.

    Deque.fromList [2..10]
        |> Deque.takeBack 3
        -- == [ 2, 3, 4 ]
-}
takeFront : Int -> Deque a -> List a
takeFront i (Deque deque) =
    List.take i deque.front ++ List.take (i - deque.sizeF) (List.reverse deque.rear)


{-| Take the last `n` members of a deque.

    Deque.fromList [2..10]
        |> Deque.takeBack 3
        -- == [ 10, 9, 8 ]
-}
takeBack : Int -> Deque a -> List a
takeBack i (Deque deque) =
    List.take i deque.rear ++ List.take (i - deque.sizeR) (List.reverse deque.front)


{-| Rebalance the deque. This is an internal function and should not normally be
called from the outside.
-}
rebalance : Deque a -> Deque a
rebalance (Deque ({ sizeF, sizeR, front, rear } as deque)) =
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
            Deque deque
        else if sizeF > balanceConstant * sizeR + 1 then
            let
                newFront =
                    List.take size1 front

                newRear =
                    rear ++ List.reverse (List.drop size1 front)
            in
                Deque
                    { deque
                        | sizeF = size1
                        , front = newFront
                        , rear = newRear
                        , sizeR = size2
                    }
        else if sizeR > balanceConstant * sizeF + 1 then
            let
                newFront =
                    front ++ List.reverse (List.drop size1 rear)

                newRear =
                    List.take size1 rear
            in
                Deque
                    { deque
                        | sizeF = size1
                        , front = newFront
                        , rear = newRear
                        , sizeR = size2
                    }
        else
            Deque deque


{-| Convert a deque to a list.
-}
toList : Deque a -> List a
toList (Deque deque) =
    deque.front ++ List.reverse deque.rear


{-| Create a deque from a list.
-}
fromList : List a -> Deque a
fromList list =
    let
        (Deque e) =
            empty
    in
        Deque { e | sizeF = List.length list, front = list }
            |> rebalance
