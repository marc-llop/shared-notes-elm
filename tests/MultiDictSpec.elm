module MultiDictSpec exposing (..)

import Dict exposing (Dict)
import Expect
import MultiDict
import Set
import Test exposing (Test)


suite : Test
suite =
    Test.describe "MultiDict"
        [ Test.describe "insert"
            [ Test.test "should insert a new key-value on a new key" <|
                \_ ->
                    let
                        actual =
                            MultiDict.insert 4 "hi" Dict.empty

                        expected =
                            Dict.fromList [ ( 4, Set.singleton "hi" ) ]
                    in
                    actual |> Expect.equal expected
            , Test.test "should insert an existing value on a new key" <|
                \_ ->
                    let
                        actual =
                            Dict.empty
                                |> MultiDict.insert 4 "hi"
                                |> MultiDict.insert 3 "hi"

                        expected =
                            Dict.fromList [ ( 4, Set.singleton "hi" ), ( 3, Set.singleton "hi" ) ]
                    in
                    actual |> Expect.equal expected
            , Test.test "should insert a new key-value on an existing key" <|
                \_ ->
                    let
                        actual =
                            Dict.empty
                                |> MultiDict.insert 4 "hi"
                                |> MultiDict.insert 4 "bye"

                        expected =
                            Dict.fromList [ ( 4, Set.fromList [ "hi", "bye" ] ) ]
                    in
                    actual |> Expect.equal expected
            , Test.test "should not insert the same key-value twice" <|
                \_ ->
                    let
                        actual =
                            Dict.empty
                                |> MultiDict.insert 4 "hi"
                                |> MultiDict.insert 4 "hi"

                        expected =
                            Dict.empty
                                |> MultiDict.insert 4 "hi"
                    in
                    actual |> Expect.equal expected
            ]
        , Test.describe "fromList"
            [ Test.test "should convert an empty list into an empty Dict" <|
                \_ ->
                    let
                        actual =
                            MultiDict.fromList []

                        expected =
                            Dict.empty
                    in
                    actual |> Expect.equal expected
            , Test.test "should convert a list into a MultiDict" <|
                \_ ->
                    let
                        actual =
                            MultiDict.fromList [ ( 1, "a" ), ( 2, "b" ), ( 1, "c" ), ( 2, "a" ), ( 4, "z" ) ]

                        expected =
                            Dict.fromList
                                [ ( 1, Set.fromList [ "a", "c" ] )
                                , ( 2, Set.fromList [ "b", "a" ] )
                                , ( 4, Set.fromList [ "z" ] )
                                ]
                    in
                    actual |> Expect.equal expected
            , Test.test "should ignore repeated key-values" <|
                \_ ->
                    let
                        actual =
                            MultiDict.fromList [ ( 1, "a" ), ( 1, "b" ), ( 1, "b" ) ]

                        expected =
                            Dict.fromList [ ( 1, Set.fromList [ "a", "b" ] ) ]
                    in
                    actual |> Expect.equal expected
            ]
        , Test.describe "toList"
            [ Test.test "should fulfill the example" <|
                \_ ->
                    let
                        actual =
                            MultiDict.fromList []
                                |> MultiDict.insert 1 "a"
                                |> MultiDict.insert 1 "b"
                                |> MultiDict.insert 1 "b"
                                |> MultiDict.toList

                        expected =
                            [ ( 1, "a" ), ( 1, "b" ) ]
                    in
                    actual |> Expect.equal expected
            , Test.test "should convert an empty Dict to an empty list" <|
                \_ ->
                    let
                        actual =
                            MultiDict.toList Dict.empty

                        expected =
                            []
                    in
                    actual |> Expect.equal expected
            , Test.test "should convert a MultiDict into a list" <|
                \_ ->
                    let
                        actual =
                            Dict.empty
                                |> MultiDict.insert 1 "a"
                                |> MultiDict.insert 1 "b"
                                |> MultiDict.insert 2 "a"
                                |> MultiDict.insert 3 "a"
                                |> MultiDict.toList

                        expected =
                            [ ( 1, "a" ), ( 1, "b" ), ( 2, "a" ), ( 3, "a" ) ]
                    in
                    actual |> Expect.equal expected
            , Test.test "should be symmetrical with fromList if the list is sorted and without repetitions" <|
                \_ ->
                    let
                        actual =
                            MultiDict.fromList expected |> MultiDict.toList

                        expectedUnsortedWithRepetitions =
                            [ ( 1, "a" ), ( 1, "b" ), ( 3, "c" ), ( 2, "c" ), ( 3, "c" ) ]

                        expected =
                            expectedUnsortedWithRepetitions
                                |> Set.fromList
                                |> Set.toList
                                |> List.sort
                    in
                    actual |> Expect.equal expected
            ]
        , Test.describe "intersectWith"
            [ Test.test "should fulfill the example" <|
                \_ ->
                    let
                        appendAll key a b =
                            key ++ a ++ b

                        dictA =
                            MultiDict.fromList [ ( "1", "a" ), ( "1", "b" ) ]

                        dictB =
                            MultiDict.fromList [ ( "1", "A" ), ( "2", "C" ) ]

                        actual =
                            MultiDict.intersectWith appendAll dictA dictB
                                |> MultiDict.toList

                        expected =
                            [ ( "1", "1aA" ) ]
                    in
                    actual |> Expect.equal expected
            , Test.test "should use each value once" <|
                \_ ->
                    let
                        appendAll key a b =
                            key ++ a ++ b

                        dictA =
                            MultiDict.fromList [ ( "1", "a" ), ( "2", "b" ) ]

                        dictB =
                            MultiDict.fromList [ ( "1", "A" ), ( "1", "B" ), ( "2", "C" ) ]

                        actual =
                            MultiDict.intersectWith appendAll dictA dictB
                                |> MultiDict.toList

                        expected =
                            [ ( "1", "1aA" ), ( "2", "2bC" ) ]
                    in
                    actual |> Expect.equal expected
            , Test.test "should intersect nothing with an empty Dict" <|
                \_ ->
                    let
                        appendAll key a b =
                            key ++ a ++ b

                        dictA =
                            MultiDict.fromList [ ( "1", "a" ), ( "2", "b" ) ]

                        dictB =
                            Dict.empty

                        actual =
                            MultiDict.intersectWith appendAll dictA dictB
                                |> MultiDict.toList

                        expected =
                            []
                    in
                    actual |> Expect.equal expected
            , Test.test "should take into account inputs order only for mapping" <|
                \_ ->
                    let
                        appendAll key a b =
                            key ++ a ++ b

                        dictA =
                            MultiDict.fromList [ ( "1", "A" ), ( "1", "B" ), ( "2", "C" ) ]

                        dictB =
                            MultiDict.fromList [ ( "1", "a" ), ( "2", "b" ) ]

                        actual =
                            MultiDict.intersectWith appendAll dictA dictB
                                |> MultiDict.toList

                        expected =
                            [ ( "1", "1Aa" ), ( "2", "2Cb" ) ]
                    in
                    actual |> Expect.equal expected
            ]
        , Test.describe "diff"
            [ Test.test "should keep only values not in the second dict" <|
                \_ ->
                    let
                        dictA =
                            MultiDict.fromList [ ( 1, "a" ), ( 1, "b" ), ( 1, "c" ), ( 2, "a" ), ( 2, "b" ), ( 3, "a" ) ]

                        dictB =
                            MultiDict.fromList [ ( 1, "b" ), ( 1, "c" ), ( 2, "a" ), ( 2, "b" ) ]

                        actual =
                            MultiDict.diff dictA dictB
                                |> MultiDict.toList

                        expected =
                            [ ( 1, "a" ), ( 3, "a" ) ]
                    in
                    actual |> Expect.equal expected
            , Test.test "should keep all values if second dict is empty" <|
                \_ ->
                    let
                        dictA =
                            MultiDict.fromList [ ( 1, "a" ), ( 1, "b" ), ( 2, "c" ) ]

                        dictB =
                            Dict.empty

                        actual =
                            MultiDict.diff dictA dictB
                                |> MultiDict.toList

                        expected =
                            [ ( 1, "a" ), ( 1, "b" ), ( 2, "c" ) ]
                    in
                    actual |> Expect.equal expected
            , Test.test "should return empty for a dict minus itself" <|
                \_ ->
                    let
                        dictA =
                            MultiDict.fromList [ ( 1, "a" ), ( 1, "b" ), ( 2, "c" ) ]

                        actual =
                            MultiDict.diff dictA dictA
                                |> MultiDict.toList

                        expected =
                            []
                    in
                    actual |> Expect.equal expected
            , Test.test "should return empty when the second dict includes all entries and more" <|
                \_ ->
                    let
                        dictA =
                            MultiDict.fromList [ ( 1, "a" ), ( 1, "b" ), ( 2, "c" ) ]

                        dictB =
                            MultiDict.fromList [ ( 1, "a" ), ( 1, "b" ), ( 1, "c" ), ( 2, "c" ), ( 2, "d" ) ]

                        actual =
                            MultiDict.diff dictA dictB
                                |> MultiDict.toList

                        expected =
                            []
                    in
                    actual |> Expect.equal expected
            , Test.test "should keep keys that keep some values" <|
                \_ ->
                    let
                        dictA =
                            MultiDict.fromList [ ( 1, "a" ), ( 1, "b" ), ( 1, "c" ), ( 2, "c" ), ( 2, "d" ) ]

                        dictB =
                            MultiDict.fromList [ ( 1, "b" ), ( 2, "e" ) ]

                        actual =
                            MultiDict.diff dictA dictB
                                |> MultiDict.toList

                        expected =
                            [ ( 1, "a" ), ( 1, "c" ), ( 2, "c" ), ( 2, "d" ) ]
                    in
                    actual |> Expect.equal expected
            ]
        ]
