module Http where
{-| A library for asynchronous HTTP requests. The central method is `send`,
which turns a signal of requests into a signal of responses. Abstractions for
both requests and responses are provided.

See the `WebSocket` library if you have very strict latency requirements.

# Sending Requests
@docs send

# Requests
@docs get, post, request
## JSON Requests
@docs getJson, postJson

# Responses
## General Responses
@docs Response, successes, failures
## HTTP Responses
@docs HttpFailure, HttpResponse
-}

import Signal (..)
import Json
import Maybe (Maybe, Just, Nothing)
import Native.Http

{-| Performs an HTTP request with the given Requests. Produces a signal
that carries the responses. -}
send : Signal (Request a) -> Signal (HttpResponse a)
send = Native.Http.send

{-| A general datatype for responses. -}
data Response a b = Success a | Failure b | Waiting

{- A signal of all successes. -}
successes : Signal (Response a b) -> Signal (Maybe a)
successes = lift (\r -> case r of
    Success a -> Just a
    _ -> Nothing)

{- A signal of all failures.

`successes` and `failures` update to `Nothing` when their signal of responses
updates to some other state. To replace all `Nothing`s with a default, use `lift
(maybe default id) (failures signal_response)`. To change on the first
`Nothing` and drop all the ones afterwards, use `lift (maybe default id) (keepIf
isJust Nothing (failures signal_response))` -}
failures  : Signal (Response a b) -> Signal (Maybe b)
failures = lift (\r -> case r of
    Failure b -> Just b
    _ -> Nothing)

{- The datatype representing failures of an HTTP request, e.g. `Http 404 "Not
Found"`. If JSON conversion fails, `NoConversion` carries the unconvertable
string. Requests for `String` or `()` never result in `NoConversion`. -}
data HttpFailure = Http Int String | NoConversion String
type HttpResponse a = Response a HttpFailure

type Request a = {
  verb : String,
  url  : String,
  body : String,
  parse : String -> Maybe a,
  headers : [(String,String)]
 }

{-| Create a customized request. Arguments are request type (get, post, put,
delete, etc.), target url, data, parser, and a list of additional headers.
Usually the parser can be `Just`.
-}
request : String -> String -> String -> (String -> Maybe a) -> [(String,String)] -> Request a
request = Request

{-| Create a GET request to the given url. -}
get : String -> Request String
get url = Request "GET" url "" Just []

{-| Create a GET request to the given url for Json. -}
getJson : String -> Request Json.Value
getJson url = Request "GET" url "" Json.fromString []

{-| Create a POST request to the given url, carrying the given data. -}
post : String -> String -> Request ()
post url body = Request "POST" url body (\_ -> Just ()) []

{-| Create a POST request to the given url, carrying the given Json. -}
postJson : String -> Json.Value -> Request ()
postJson url json = Request "POST" url (Json.toString "" json) (\_ -> Just ()) [("Content-Type", "application/json")]
