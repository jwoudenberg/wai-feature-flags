module CaseConversion exposing (toSpaceCase)

import Parser exposing (..)


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
