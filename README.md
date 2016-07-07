elm-deque
=========

A Deque (double-ended queue) in Elm.

A deque (the name is pronounced "deck" and is short for "double-ended queue") is a data type for which elements can be efficiently added or removed from either the front or the back.

Internally, this is a head-tail linked list, modelled after [deque in Haskell](https://hackage.haskell.org/package/deque-0.1.12/docs/Data-Dequeue.html) which
in turn is based on Chris Okasaki's Purely Functional Data Structures. A head-tail linked list is based on two lists: one for the head and one for the tail.
This means that pop and push on either side are operations on the front portion of an elm list, which is very efficient (`O(n)`).

# Example 

```elm
import Deque

example1 = 
    Deque.empty 
        |> Deque.pushFront 4
        |> Deque.pushBack 2
        |> Deque.popFront |> snd 
        |> Deque.popBack  |> fst
        |> toString 
        -- Just 2

example2 = 
    Deque.fromList [2..10] 
        |> Deque.popFront
        |> (\(elem, deque) -> Maybe.map (\list -> elem :: list) (Deque.takeBack 3 d))
        |> toString
        -- Just 2
