module IdentifiersSpec exposing (suite)

import Expect
import Fuzz
import Identifiers exposing (wordGenerator, notebookIdRegex)
import Random
import Test exposing (Test)
import Tuple
import Regex


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
                    Regex.contains notebookIdRegex "abcde-fghij-12345"
                        |> Expect.true "Expected notebookIdRegex to recognise abcde-fghij-12345"
            , Test.test "fails for incorrect IDs" <|
                \_ ->
                    Regex.contains notebookIdRegex "abce-fghij-12345"
                        |> Expect.false "Expected notebookIdRegex to not recognise abce-fghij-12345"
            , Test.test "fails for IDs with less parts" <|
                \_ ->
                    Regex.contains notebookIdRegex "abcde-12345"
                        |> Expect.false "Expected notebookIdRegex to not recognise abcde-12345"
            , Test.test "fails for the empty ID" <|
                \_ ->
                    Regex.contains notebookIdRegex ""
                        |> Expect.false "Expected notebookIdRegex to not recognise "
            , Test.test "fails for slash ID" <|
                \_ ->
                    Regex.contains notebookIdRegex "/"
                        |> Expect.false "Expected notebookIdRegex to not recognise '/'"
            ]
        ]
