module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Persisted exposing (Persisted, changed, init)
import Time
import Url.Builder


type Msg
    = LoadFlags
    | LoadedFlags (Dict FlagName Percentage)
    | SetFlag FlagName Percentage


type Model
    = Loading
    | Loaded (Dict FlagName (Persisted Percentage))


type alias FlagName =
    String


type Percentage
    = Percentage Int


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( Loading, loadFlags )
        , view = \_ -> { title = "Feature Flags", body = [] }
        , update = update
        , subscriptions = \_ -> Time.every (5 * 1000) (\_ -> LoadFlags)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadFlags ->
            ( model, loadFlags )

        LoadedFlags updatedFlags ->
            case model of
                Loading ->
                    ( Loaded (Dict.map (\_ -> Persisted.init) updatedFlags)
                    , Cmd.none
                    )

                Loaded oldFlags ->
                    ( Loaded <|
                        Dict.merge
                            (\flag oldValue newFlags ->
                                Dict.insert flag oldValue newFlags
                            )
                            (\flag oldValue newValue newFlags ->
                                Dict.insert flag (Persisted.persist newValue oldValue) newFlags
                            )
                            (\flag firstValue newFlags ->
                                Dict.insert flag (Persisted.init firstValue) newFlags
                            )
                            oldFlags
                            updatedFlags
                            Dict.empty
                    , Cmd.none
                    )

        SetFlag flagName newPercentage ->
            case model of
                Loading ->
                    ( Loading, Cmd.none )

                Loaded flags ->
                    ( Loaded <|
                        Dict.update
                            flagName
                            (Maybe.map (\oldPercentage -> Persisted.change newPercentage oldPercentage))
                            flags
                    , saveFlag flagName newPercentage
                    )


loadFlags : Cmd Msg
loadFlags =
    Http.get
        { url = Url.Builder.relative [ "flags" ] []
        , expect =
            Http.expectJson
                (\res ->
                    case res of
                        Ok x ->
                            LoadedFlags x

                        Err _ ->
                            LoadedFlags Dict.empty
                )
                flagsDecoder
        }


flagsDecoder : Decoder (Dict FlagName Percentage)
flagsDecoder =
    Decode.dict (Decode.map Percentage Decode.int)


saveFlag : FlagName -> Percentage -> Cmd Msg
saveFlag flagName (Percentage percentage) =
    Http.request
        { method = "PUT"
        , headers = []
        , url = Url.Builder.relative [ "flags", flagName ] []
        , body = Http.jsonBody (Encode.int percentage)
        , expect =
            Http.expectWhatever
                (\res ->
                    case res of
                        Ok () ->
                            LoadedFlags (Dict.singleton flagName (Percentage percentage))

                        Err _ ->
                            LoadedFlags Dict.empty
                )
        , timeout = Nothing
        , tracker = Nothing
        }
