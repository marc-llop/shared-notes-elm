module IdentifiersSpec exposing (suite)

import Expect
import Fuzz
import Identifiers exposing (wordGenerator)
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
        ]
