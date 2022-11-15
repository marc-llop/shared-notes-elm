module IdentifiersSpec exposing (suite)

import Expect
import Fuzz
import Identifiers exposing (notebookIdToString, parseNotebookId, wordGenerator)
import Random
import Test exposing (Test)
import Tuple


generateWord : Int -> String
generateWord seed =
    Random.initialSeed seed
        |> Random.step wordGenerator
        |> Tuple.first


suite : Test
suite =
    Test.describe "Identifiers"
        [ Test.describe "wordGenerator"
            [ Test.fuzz Fuzz.int "Generated Id should have five characters" <|
                \fuzz ->
                    generateWord fuzz
                        |> String.length
                        |> Expect.equal 5
            , Test.fuzz Fuzz.int "All characters should be alphanumeric" <|
                \fuzz ->
                    generateWord fuzz
                        |> (\word -> ( word, String.toList word ))
                        |> (\( word, chars ) ->
                                List.foldl
                                    (\char expectResult ->
                                        if Char.isAlphaNum char then
                                            expectResult

                                        else
                                            Expect.fail ("Found incorrect character " ++ String.fromList [ char ] ++ " in word " ++ word)
                                    )
                                    Expect.pass
                                    chars
                           )
            ]
        , Test.describe "notebookIdRegex"
            [ Test.test "passes for correct IDs" <|
                \_ ->
                    parseNotebookId "abcde-fghij-12345"
                        |> Expect.ok
            , Test.test "fails for incorrect IDs" <|
                \_ ->
                    parseNotebookId "abce-fghij-12345"
                        |> Expect.err
            , Test.test "fails for IDs with less parts" <|
                \_ ->
                    parseNotebookId "abcde-12345"
                        |> Expect.err
            , Test.test "fails for the empty ID" <|
                \_ ->
                    parseNotebookId ""
                        |> Expect.err
            , Test.test "fails for slash ID" <|
                \_ ->
                    parseNotebookId "/"
                        |> Expect.err
            ]
        , Test.describe "parseNotebookId"
            [ Test.test "should be the inverse of notebookIdToString" <|
                \_ ->
                    parseNotebookId "hello-there-12345"
                        |> Result.map notebookIdToString
                        |> Result.withDefault ""
                        |> Expect.equal "hello-there-12345"
            ]
        ]
