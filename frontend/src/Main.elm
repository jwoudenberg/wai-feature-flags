module Main exposing (main)

import Browser
import Char
import Css
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import Html.Styled.Lazy
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
        , view =
            \model ->
                { title = "Feature Flags"
                , body = [ Html.toUnstyled (view model) ]
                }
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


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.css
            [ Css.textAlign Css.center
            , Css.fontFamilies [ "Helvetica Neue", "Helvetica", "Arial", "sans-serif" ]
            , Css.color (Css.hex "#333")
            , Css.backgroundColor (Css.hex "#fefefe")
            , Css.position Css.absolute
            , Css.minHeight (Css.vh 100)
            , Css.width (Css.vw 100)
            ]
        ]
        (Html.h1 [] [ Html.text "Feature Flags" ]
            :: (case model of
                    Loading ->
                        [ Html.text "Loading..." ]

                    Loaded flags ->
                        Html.p [] [ Html.text "Every feature flag can be completely enabled, completely disabled, or enabled for a percentage of traffic." ]
                            :: Dict.values (Dict.map viewFlag flags)
               )
        )


viewFlag : FlagName -> Persisted Percentage -> Html Msg
viewFlag name percentage =
    let
        (Percentage current) =
            Persisted.value percentage
    in
    Html.section
        [ Attr.css
            [ case Persisted.value percentage of
                Percentage 0 ->
                    Css.batch []

                Percentage 100 ->
                    Css.batch
                        [ Css.backgroundColor (Css.hex "#C1F0C1")
                        ]

                Percentage _ ->
                    Css.batch
                        [ Css.backgroundColor (Css.hex "#F0EEC1")
                        ]
            , Css.padding (Css.px 10)
            ]
        ]
        [ Html.h2
            [ Attr.css
                [ Css.fontWeight Css.normal
                , Css.fontSize (Css.em 1.4)
                , Css.margin4 Css.zero Css.zero (Css.em 0.5) Css.zero
                ]
            ]
            [ Html.Styled.Lazy.lazy (Html.text << toSpaceCase) name ]
        , Html.button
            [ Attr.css [ inputStyle ]
            , Events.onClick (SetFlag name (Percentage 0))
            ]
            [ Html.text "❌" ]
        , Html.input
            [ Attr.value (String.fromInt current)
            , Attr.type_ "number"
            , Attr.min "0"
            , Attr.max "100"
            , Attr.step "1"
            , Attr.css
                [ inputStyle
                , Css.width (Css.px 60)
                , Css.margin2 Css.zero (Css.px 10)
                , Css.after
                    [ Css.property "content" "%"
                    ]
                ]
            , Events.on "input" (Decode.map (handleFlagValue name) targetValueNumber)
            ]
            []
        , Html.button
            [ Attr.css [ inputStyle ]
            , Events.onClick (SetFlag name (Percentage 100))
            ]
            [ Html.text "✅" ]
        ]


inputStyle : Css.Style
inputStyle =
    Css.batch
        [ Css.fontSize (Css.em 1.1)
        , Css.height (Css.em 1.6)
        , Css.border3 (Css.px 1) Css.solid (Css.hex "#ccc")
        , Css.borderRadius (Css.px 2)
        , Css.textAlign Css.center
        , Css.boxSizing Css.borderBox
        , Css.backgroundColor (Css.hex "#fefefe")
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
