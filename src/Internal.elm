module Internal exposing (Deque, empty, filter, first, foldl, foldr, fromList, isEmpty, last, length, map, member, popBack, popFront, rebalance, takeBack, takeFront, toList)

{-| The deque datatype

Deque equality with `(==)` is unreliable (equivalent deques can have a different distribution of elements between the back
and the front) and should not be used.

-}


type alias Deque a =
    { sizeF : Int
    , front : List a
    , sizeR : Int
    , rear : List a
    }


empty : Deque a
empty =
    { sizeF = 0
    , front = []
    , sizeR = 0
    , rear = []
    }


{-| Extract the first element of a deque
-}
first : Deque a -> Maybe a
first deque =
    case ( deque.front, deque.rear ) of
        ( [], [ x ] ) ->
            Just x

        _ ->
            List.head deque.front


{-| Extract the last element of a deque.
-}
last : Deque a -> Maybe a
last deque =
    case ( deque.front, deque.rear ) of
        ( [ x ], [] ) ->
            Just x

        _ ->
            List.head deque.rear


{-| Gives Maybe the first element, and the deque without the first element.
If there are no elements, the empty deque is returned.
-}
popFront : Deque a -> ( Maybe a, Deque a )
popFront ({ front, rear } as deque) =
    case ( front, rear ) of
        ( [], [] ) ->
            ( Nothing, empty )

        ( [], [ x ] ) ->
            ( Just x, empty )

        ( [], _ ) ->
            -- Debug.crash "Deque is too far unbalanced"
            ( Nothing, empty )

        ( f :: fs, _ ) ->
            ( Just f
            , { sizeF = deque.sizeF - 1, front = fs, sizeR = deque.sizeR, rear = deque.rear }
                |> rebalance
            )


{-| Gives Maybe the last element, and the deque without the last element.
If there are no elements, the empty deque is returned.
-}
popBack : Deque a -> ( Maybe a, Deque a )
popBack ({ front, rear } as deque) =
    case ( front, rear ) of
        ( [], [] ) ->
            ( Nothing, empty )

        ( [ x ], [] ) ->
            ( Just x, empty )

        ( _, [] ) ->
            -- Debug.crash "Deque is too far unbalanced"
            ( Nothing, empty )

        ( _, r :: rs ) ->
            ( Just r
            , { sizeR = deque.sizeR - 1, rear = rs, sizeF = deque.sizeF, front = deque.front }
                |> rebalance
            )


{-| Take the first `n` members of a deque.

    AbstractDeque record.fromList [2..10]
        |> AbstractDeque record.takeBack 3
        -- == [ 2, 3, 4 ]

-}
takeFront : Int -> Deque a -> List a
takeFront i deque =
    List.take i deque.front ++ List.take (i - deque.sizeF) (List.reverse deque.rear)


{-| Take the last `n` members of a deque.

    AbstractDeque record.fromList [2..10]
        |> AbstractDeque record.takeBack 3
        -- == [ 10, 9, 8 ]

-}
takeBack : Int -> Deque a -> List a
takeBack i deque =
    List.take i deque.rear ++ List.take (i - deque.sizeR) (List.reverse deque.front)


{-| Determine if a deque is empty.
-}
isEmpty : Deque a -> Bool
isEmpty deque =
    length deque == 0


{-| Figure out whether a deque contains a value.
-}
member : a -> Deque a -> Bool
member elem deque =
    List.member elem deque.front || List.member elem deque.rear


{-| Determine the length of a list.
-}
length : Deque a -> Int
length deque =
    deque.sizeF + deque.sizeR


{-| Apply a function to all elements in a deque.
-}
map : (a -> b) -> Deque a -> Deque b
map f deque =
    { front = List.map f deque.front
    , rear = List.map f deque.rear
    , sizeF = deque.sizeF
    , sizeR = deque.sizeR
    }


{-| Keep an element when it satisfies a predicate.
-}
filter : (a -> Bool) -> Deque a -> Deque a
filter p deque =
    let
        newFront =
            List.filter p deque.front

        newRear =
            List.filter p deque.rear
    in
    { sizeF = List.length newFront
    , front = newFront
    , sizeR = List.length newRear
    , rear = newRear
    }
        |> rebalance


{-| Fold over the deque from left to right (highest priority to lowest priority).
-}
foldl : (a -> b -> b) -> b -> Deque a -> b
foldl f initial deque =
    List.foldl f initial deque.front
        |> (\initial_ -> List.foldr f initial_ deque.rear)


{-| Fold over the deque from right to left (lowest priority to highest priority).
-}
foldr : (a -> b -> b) -> b -> Deque a -> b
foldr f initial deque =
    List.foldl f initial deque.rear
        |> (\initial_ -> List.foldr f initial_ deque.front)


{-| Convert a deque to a list.
-}
toList : Deque a -> List a
toList deque =
    deque.front ++ List.reverse deque.rear


{-| Create a deque from a list.
-}
fromList : List a -> Deque a
fromList list =
    { sizeF = List.length list, front = list, sizeR = 0, rear = [] }
        |> rebalance


{-| Rebalance the deque. This is an internal function and should not normally be
called from the outside.
-}
rebalance : Deque a -> Deque a
rebalance ({ sizeF, sizeR, front, rear } as deque) =
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
        { sizeF = size1
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
        { sizeF = size1
        , front = newFront
        , rear = newRear
        , sizeR = size2
        }

    else
        deque
