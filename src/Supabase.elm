module Supabase exposing (getSupabase, postSupabase, patchSupabase)

import Http exposing (emptyBody, jsonBody, task, header)
import Http.Tasks exposing (resolveJson)
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Task exposing (Task)

clientKey : String
clientKey = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6ImFrZ3dvZ3ppdHJvcXNrZG1nd3FqIiwicm9sZSI6ImFub24iLCJpYXQiOjE2Njg2MTg2MDksImV4cCI6MTk4NDE5NDYwOX0.9GQWV1W8wR5vZgIcgr1wSCIGOeC5poAgQdQLNX83bQI"

headers : List Http.Header
headers =
    [ header "apikey" clientKey
    ]


supabaseUrl : String
supabaseUrl =
    "https://akgwogzitroqskdmgwqj.supabase.co/rest/v1/"


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
        , headers = headers ++ [ header "Prefer" "return=representation" ]
        , url = endpoint path
        , body = jsonBody body
        , resolver = resolveJson decoder
        , timeout = Nothing
        }


patchSupabase : { path : String, body : Value, decoder : Decoder a } -> Task Http.Error a
patchSupabase { path, body, decoder } =
    task
        { method = "PATCH"
        , headers = headers ++ [ header "Prefer" "return=representation" ]
        , url = endpoint path
        , body = jsonBody body
        , resolver = resolveJson decoder
        , timeout = Nothing
        }
