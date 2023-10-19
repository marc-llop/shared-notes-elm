module MultiDict exposing (MultiDict, diff, fromList, insert, intersectWith, toList)

import Dict exposing (Dict)
import Set exposing (Set)


{-| A Dict that can hold a set of values for each key.
-}
type alias MultiDict comparableA comparableB =
    Dict comparableA (Set comparableB)


{-| Insert a key-value pair into a dictionary as key-Set(value).
Adds the value to the existing set in that key when there is a collision.
Note that sets do not have repeated values.
-}
insert : comparableA -> comparableB -> MultiDict comparableA comparableB -> MultiDict comparableA comparableB
insert key value dict =
    let
        insertInSet : comparable -> Maybe (Set comparable) -> Maybe (Set comparable)
        insertInSet val maybeSet =
            case maybeSet of
                Just set ->
                    Just (Set.insert val set)

                Nothing ->
                    Just (Set.singleton val)
    in
    Dict.update key (insertInSet value) dict


{-| Convert an association list into a dictionary,
putting all values with the same key in the set for that key.
-}
fromList : List ( comparableA, comparableB ) -> MultiDict comparableA comparableB
fromList l =
    case l of
        [] ->
            Dict.empty

        ( key, x ) :: rest ->
            insert key x (fromList rest)


{-| Convert a dictionary into an association list of key-value pairs,
sorted by keys.

    MultiDict.fromList []
        |> MultiDict.insert 1 "a"
        |> MultiDict.insert 1 "b"
        |> MultiDict.insert 1 "b"
        |> MultiDict.toList
        == [ ( 1, "a" ), ( 1, "b" ) ]

-}
toList : MultiDict a b -> List ( a, b )
toList dict =
    let
        setTupleToList : ( a, Set b ) -> List ( a, b )
        setTupleToList ( key, set ) =
            Set.toList set
                |> List.map (Tuple.pair key)
    in
    Dict.toList dict
        |> List.concatMap setTupleToList


setMap2 : (a -> b -> comparable) -> Set a -> Set b -> Set comparable
setMap2 fn setA setB =
    List.map2 fn (Set.toList setA) (Set.toList setB)
        |> Set.fromList


{-| Keep a key-value pair when its key appears in the second dictionary.
Each value is only used once.
Map the resulting value combining both pairs using the provided function.

    appendAll key a b = key ++ a ++ b

    dictA = MultiDict.fromList [ ("1", "a"), ("1", "b") ]

    dictB = MultiDict.fromList [ ("1", "A"), ("2", "C") ]

    MultiDict.intersectWith appendAll dictA dictB
        |> MultiDict.toList
        == [ ("1", "1aA") ]

-}
intersectWith :
    (comparableKey -> comparableA -> comparableB -> comparableC)
    -> MultiDict comparableKey comparableA
    -> MultiDict comparableKey comparableB
    -> MultiDict comparableKey comparableC
intersectWith fn dictA dictB =
    let
        forget : comparable -> any -> MultiDict i c -> MultiDict i c
        forget _ _ dict =
            dict

        combine :
            comparableKey
            -> Set comparableA
            -> Set comparableB
            -> MultiDict comparableKey comparableC
            -> MultiDict comparableKey comparableC
        combine key setA setB dict =
            Dict.insert key (setMap2 (fn key) setA setB) dict
    in
    Dict.merge
        -- Keys that only appear in the left dictionary:
        forget
        -- Keys that appear in both:
        combine
        -- Keys that only appear in the right dictionary:
        forget
        dictA
        dictB
        Dict.empty


{-| Keep a key-value pair when it does not appear in the second dictionary.
-}
diff :
    MultiDict comparableKey comparableA
    -> MultiDict comparableKey comparableA
    -> MultiDict comparableKey comparableA
diff dictA dictB =
    let
        forget : comparable -> any -> MultiDict i c -> MultiDict i c
        forget _ _ dict =
            dict

        keep :
            comparableKey
            -> Set comparableA
            -> MultiDict comparableKey comparableA
            -> MultiDict comparableKey comparableA
        keep key value dict =
            Dict.insert key value dict

        diffSets :
            comparableKey
            -> Set comparableA
            -> Set comparableA
            -> MultiDict comparableKey comparableA
            -> MultiDict comparableKey comparableA
        diffSets key setA setB dict =
            Set.diff setA setB
                |> (\value -> Dict.insert key value dict)
    in
    Dict.merge
        -- Keys that only appear in the left dictionary:
        keep
        -- Keys that appear in both:
        diffSets
        -- Keys that only appear in the right dictionary:
        forget
        dictA
        dictB
        Dict.empty
