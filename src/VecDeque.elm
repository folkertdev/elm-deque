module VecDeque exposing (..)

import Array exposing (..)
import Bitwise exposing (and)


type VecDeque a
    = VecDeque
        { tail :
            Int
            -- first element that could be read
        , head :
            Int
            -- where data should be written
        , buf :
            Array a
            -- buffer
        }


vecDeque t h b =
    VecDeque
        { tail = t
        , head = h
        , buf = b
        }


empty : VecDeque a
empty =
    vecDeque 0 0 Array.empty


isEmpty : VecDeque a -> Bool
isEmpty deque =
    length deque == 0


isFull : VecDeque a -> Bool
isFull (VecDeque deque) =
    (Array.length deque.buf) - (length (VecDeque deque)) == 1


{-| Get the element at a specified index, with the index wrapped
around the current capacity (i.e. maxcap  becomes 0, -1 becomes maxcap - 1
-}
getWrapped : Int -> Array a -> Int
getWrapped index array =
    index % (Array.length array)


wrapAdd : Int -> Int -> Array a -> Int
wrapAdd a b array =
    getWrapped (a + b) array


wrapSub : Int -> Int -> Array a -> Int
wrapSub a b array =
    getWrapped (a - b) array


count : Int -> Int -> Int -> Int
count tail head size =
    (head - tail) `and` (size - 1)


length : VecDeque a -> Int
length (VecDeque { tail, head, buf }) =
    count tail head (Array.length buf)


clear : VecDeque a -> VecDeque a
clear (VecDeque { tail, head }) =
    vecDeque tail head Array.empty


front : VecDeque a -> Maybe a
front (VecDeque deque) =
    Array.get 0 deque.buf


back : VecDeque a -> Maybe a
back (VecDeque deque) =
    let
        size =
            length (VecDeque deque)
    in
        Array.get (size - 1) deque.buf


popFront : VecDeque a -> Maybe ( a, VecDeque a )
popFront (VecDeque deque) =
    let
        tail =
            wrapAdd deque.tail 1 deque.buf

        elem =
            Array.get deque.tail deque.buf

        newDeque =
            vecDeque tail deque.head deque.buf
    in
        Maybe.map (\e -> ( e, newDeque )) elem


popBack : VecDeque a -> Maybe ( a, VecDeque a )
popBack (VecDeque deque) =
    let
        head =
            wrapSub deque.head 1 deque.buf

        elem =
            Array.get head deque.buf

        newDeque =
            vecDeque deque.tail head deque.buf
    in
        Maybe.map (\e -> ( e, newDeque )) elem
