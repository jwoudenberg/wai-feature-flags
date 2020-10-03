module Main exposing (main)

import Browser
import Char
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Lazy
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Parser exposing ((|.), (|=), Parser)
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
        { init = \_ -> ( Loading, sendLoadFlagsRequest )
        , view = \model -> { title = "Feature Flags", body = view model }
        , update = update
        , subscriptions = \_ -> Time.every (5 * 1000) (\_ -> LoadFlags)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadFlags ->
            ( model, sendLoadFlagsRequest )

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
            let
                newModel =
                    setFlag flagName newPercentage model
            in
            ( newModel
            , saveFlag flagName newModel
            )


view : Model -> List (Html Msg)
view model =
    Html.h1 [] [ Html.text "Feature Flags" ]
        :: (case model of
                Loading ->
                    [ Html.text "Loading..." ]

                Loaded flags ->
                    Dict.values (Dict.map viewFlag flags)
           )


viewFlag : FlagName -> Persisted Percentage -> Html Msg
viewFlag name percentage =
    let
        (Percentage current) =
            Persisted.value percentage
    in
    Html.section []
        [ Html.h2 [] [ Html.Lazy.lazy (Html.text << toSpaceCase) name ]
        , Html.button
            [ Events.onClick (SetFlag name (Percentage 0))
            ]
            [ Html.text "❌" ]
        , Html.input
            [ Attr.value (String.fromInt current)
            , Attr.type_ "number"
            , Attr.min "0"
            , Attr.max "100"
            , Attr.step "1"
            , Events.on "input" (Decode.map (handleFlagValue name) targetValueNumber)
            ]
            []
        , Html.button
            [ Events.onClick (SetFlag name (Percentage 100))
            ]
            [ Html.text "✅" ]
        ]


targetValueNumber : Decoder Float
targetValueNumber =
    Events.targetValue
        |> Decode.andThen
            (\str ->
                case String.toFloat str of
                    Nothing ->
                        Decode.fail "Expected a number"

                    Just float ->
                        Decode.succeed float
            )


handleFlagValue : FlagName -> Float -> Msg
handleFlagValue flagName newValue =
    round newValue
        |> clamp 0 100
        |> Percentage
        |> SetFlag flagName


sendLoadFlagsRequest : Cmd Msg
sendLoadFlagsRequest =
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


setFlag : FlagName -> Percentage -> Model -> Model
setFlag flagName newPercentage model =
    case model of
        Loading ->
            Loading

        Loaded flags ->
            Dict.update
                flagName
                (Maybe.map (\oldPercentage -> Persisted.change newPercentage oldPercentage))
                flags
                |> Loaded


saveFlag : FlagName -> Model -> Cmd Msg
saveFlag flagName model =
    case model of
        Loading ->
            Cmd.none

        Loaded flags ->
            case Dict.get flagName flags of
                Nothing ->
                    Cmd.none

                Just percentage ->
                    if Persisted.changed percentage then
                        sendSaveFlagRequest flagName (Persisted.value percentage)

                    else
                        Cmd.none


sendSaveFlagRequest : FlagName -> Percentage -> Cmd Msg
sendSaveFlagRequest flagName (Percentage percentage) =
    Http.request
        { method = "PUT"
        , headers = []
        , url = Url.Builder.relative [ "flags", flagName ] []
        , body = Http.jsonBody (Encode.int percentage)
        , expect = Http.expectWhatever (\_ -> LoadFlags)
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Take a string that might be snake-cased or camel-cased and make it
presentable to humans by adding spaces.
-}
toSpaceCase : String -> String
toSpaceCase original =
    words original
        |> List.reverse
        |> List.map capitalize
        |> String.join " "


words : String -> List String
words string =
    let
        parser =
            Parser.loop
                []
                (\words_ ->
                    Parser.oneOf
                        [ Parser.map (\word_ -> Parser.Loop (Debug.log "so far" (word_ :: words_))) word
                        , Parser.succeed (Parser.Done words_)
                        ]
                )
    in
    case Parser.run parser string of
        -- We don't expect to ever end up here.
        Err _ ->
            [ string ]

        Ok words_ ->
            words_


word : Parser String
word =
    Parser.succeed identity
        |. Parser.chompWhile isWhitespace
        |= (Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.chompIf (not << isWhitespace)
                    |. Parser.chompWhile (\c -> not (isWhitespace c) && not (Char.isUpper c))
           )
        |. Parser.chompWhile isWhitespace


isWhitespace : Char -> Bool
isWhitespace c =
    c == ' ' || c == '\n' || c == '\u{000D}' || c == '_'


capitalize : String -> String
capitalize original =
    case String.uncons original of
        Nothing ->
            original

        Just ( first, rest ) ->
            String.cons (Char.toUpper first) rest
