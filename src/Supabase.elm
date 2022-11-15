module Supabase exposing (..)

import Http exposing (emptyBody, jsonBody, task)
import Http.Tasks exposing (resolveJson)
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Task exposing (Task)


headers : List Http.Header
headers =
    []


supabaseUrl : String
supabaseUrl =
    ""


endpoint : String -> String
endpoint path =
    if String.startsWith "/" path then
        supabaseUrl ++ path

    else
        supabaseUrl ++ "/" ++ path


getSupabase : { path : String, decoder : Decoder a } -> Task Http.Error a
getSupabase { path, decoder } =
    task
        { method = "GET"
        , headers = headers
        , url = endpoint path
        , body = emptyBody
        , resolver = resolveJson decoder
        , timeout = Nothing
        }


postSupabase : { path : String, body : Value, decoder : Decoder a } -> Task Http.Error a
postSupabase { path, body, decoder } =
    task
        { method = "POST"
        , headers = headers
        , url = endpoint path
        , body = jsonBody body
        , resolver = resolveJson decoder
        , timeout = Nothing
        }


patchSupabase : { path : String, body : Value, decoder : Decoder a } -> Task Http.Error a
patchSupabase { path, body, decoder } =
    task
        { method = "PATCH"
        , headers = headers
        , url = endpoint path
        , body = jsonBody body
        , resolver = resolveJson decoder
        , timeout = Nothing
        }
