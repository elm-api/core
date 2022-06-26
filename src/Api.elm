module Api exposing
    ( Error(..)
    , RequestBuilder
    , Response
    , createRequest
    , delete
    , get
    , noBodyDecoder
    , patch
    , post
    , rawStringDecoder
    , send
    , withBody
    , withCredentials
    , withHeader
    , withTimeout
    , withTracker
    )

import Http


type Error decoderError error
    = BadStatus Http.Metadata error
    | BadUrl error
    | DecoderError decoderError
    | NetworkError
    | Timeout


type alias Decoder error data =
    String -> Result error data


rawStringDecoder : Decoder Never String
rawStringDecoder =
    Ok << identity


noBodyDecoder : Decoder Never ()
noBodyDecoder =
    \_ -> Ok <| ()


type alias RequestBuilder data error decoderError =
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , decoder : Decoder decoderError data
    , errorDecoder : Decoder decoderError error
    , timeout : Maybe Float
    , withCredentials : Bool
    , tracker : Maybe String
    }


get : String -> Decoder decoderError data -> Decoder decoderError error -> RequestBuilder data error decoderError
get =
    createRequest "GET"


post : String -> Decoder decoderError data -> Decoder decoderError error -> RequestBuilder data error decoderError
post =
    createRequest "POST"


delete : String -> Decoder decoderError data -> Decoder decoderError error -> RequestBuilder data error decoderError
delete =
    createRequest "DELETE"


patch : String -> Decoder decoderError data -> Decoder decoderError error -> RequestBuilder data error decoderError
patch =
    createRequest "PATCH"


createRequest : String -> String -> Decoder decoderError data -> Decoder decoderError error -> RequestBuilder data error decoderError
createRequest method url decoder errorDecoder =
    { method = method
    , headers = []
    , url = url
    , body = Http.emptyBody
    , decoder = decoder
    , errorDecoder = errorDecoder
    , timeout = Nothing
    , withCredentials = False
    , tracker = Nothing
    }


withHeader : String -> String -> RequestBuilder data error decoderError -> RequestBuilder data error decoderError
withHeader key value builder =
    let
        header : Http.Header
        header =
            Http.header key value
    in
    { builder | headers = header :: builder.headers }


withBody : Http.Body -> RequestBuilder data error decoderError -> RequestBuilder data error decoderError
withBody body builder =
    { builder | body = body }


withTimeout : Float -> RequestBuilder data error decoderError -> RequestBuilder data error decoderError
withTimeout timeout builder =
    { builder | timeout = Just timeout }


withCredentials : RequestBuilder data error decoderError -> RequestBuilder data error decoderError
withCredentials builder =
    { builder | withCredentials = True }


withTracker : String -> RequestBuilder data error decoderError -> RequestBuilder data error decoderError
withTracker tracker builder =
    { builder | tracker = Just tracker }


type alias HttpResponse data =
    Result Http.Error data


except : Decoder decoderError error -> Decoder decoderError data -> Http.Expect (Response data error decoderError)
except errorDecoder dataDecoder =
    Http.expectStringResponse identity <|
        \response ->
            case response of
                Http.BadUrl_ string ->
                    Err <|
                        case errorDecoder string of
                            Ok err ->
                                BadUrl err

                            Err err2 ->
                                DecoderError err2

                Http.BadStatus_ metadata body ->
                    Err <|
                        case errorDecoder body of
                            Ok err ->
                                BadStatus metadata err

                            Err err2 ->
                                DecoderError err2

                Http.Timeout_ ->
                    Err Timeout

                Http.NetworkError_ ->
                    Err NetworkError

                Http.GoodStatus_ _ body ->
                    case dataDecoder body of
                        Ok val ->
                            Ok val

                        Err err ->
                            Err <| DecoderError err


type alias Response data error decoderError =
    Result (Error decoderError error) data


send : RequestBuilder data error decoderError -> Cmd (Response data error decoderError)
send builder =
    Http.request
        { method = builder.method
        , url = builder.url
        , headers = builder.headers
        , body = builder.body
        , timeout = builder.timeout
        , tracker = builder.tracker
        , expect = except builder.errorDecoder builder.decoder
        }
