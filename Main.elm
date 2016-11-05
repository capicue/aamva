module Main exposing (..)

import Char
import Html exposing (Html, br, div, input, table, td, text, th, tr)
import Html.App exposing (beginnerProgram)
import Html.Attributes exposing (placeholder, style)
import Html.Events exposing (onInput)
import Parser exposing (Parser, and, andMap, map, (<*), (*>))
import Parser.Char
import Parser.Number
import String


-- MODEL


type alias Model =
    { input : String }


init : Model
init =
    { input = "" }



-- UPDATE


type Msg
    = UpdateInput String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateInput input ->
            { model | input = input }



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "margin", "80px auto" )
            , ( "width", "1000px" )
            ]
        ]
        [ input
            [ onInput UpdateInput
            , style
                [ ( "width", "100%" )
                ]
            , placeholder "DL/ID"
            ]
            []
        , viewResult model
        ]


viewResult : Model -> Html Msg
viewResult model =
    case (Parser.parse content model.input) of
        Ok result ->
            table
                [ style
                    [ ( "margin", "20px 0px" ) ]
                ]
                [ tr
                    []
                    [ th
                        []
                        [ text "State/Province" ]
                    , td
                        []
                        [ text result.stateOrProvince ]
                    ]
                , tr
                    []
                    [ th
                        []
                        [ text "City" ]
                    , td
                        []
                        [ text result.city ]
                    ]
                , tr
                    []
                    [ th
                        []
                        [ text "Name" ]
                    , td
                        []
                        [ text <| result.name.familyName ++ ", " ++ result.name.givenName ++ " " ++ result.name.suffix ]
                    ]
                , tr
                    []
                    [ th
                        []
                        [ text "Address" ]
                    , td
                        []
                        [ text <| String.join "\n" result.address ]
                    ]
                , tr
                    []
                    [ th
                        []
                        [ text "Identification Number" ]
                    , td
                        []
                        [ text result.identificationNumber ]
                    ]
                , tr
                    []
                    [ th
                        []
                        [ text "Drivers License ID" ]
                    , td
                        []
                        [ text result.driversLicenseID ]
                    ]
                , tr
                    []
                    [ th
                        []
                        [ text "Expiration Date" ]
                    , td
                        []
                        [ text <| result.expirationDate.month ++ "/" ++ result.expirationDate.year ]
                    ]
                , tr
                    []
                    [ th
                        []
                        [ text "Birth Date" ]
                    , td
                        []
                        [ text <| result.birthDate.year ++ "/" ++ result.birthDate.month ++ "/" ++ result.birthDate.day ]
                    ]
                ]

        Err err ->
            if String.isEmpty model.input then
                text ""
            else
                div
                    [ style
                        [ ( "margin", "20px 0px" )
                        , ( "font-family", "monospace" )
                        , ( "color", "grey" )
                        ]
                    ]
                    [ text "Sanitized input for debugging:"
                    , br [] []
                    , text <| sanitize model.input
                    ]



-- PARSER HELPERS


count : Int -> Parser a -> Parser (List a)
count n parser =
    case (List.repeat n parser) of
        x :: xs ->
            (map (List.repeat 1) x)
                `andMap` (map (++) (count (n - 1) parser))

        [] ->
            Parser.succeed []


punctuation : Parser Char
punctuation =
    Parser.choice
        [ Parser.symbol '.'
        , Parser.symbol '-'
        , Parser.symbol '\''
        ]


space : Parser Char
space =
    Parser.symbol ' '


digit : Parser Char
digit =
    Parser.choice <|
        List.map Parser.symbol [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]



-- PARSING


type alias Content =
    { stateOrProvince : StateOrProvince
    , city : City
    , name : Name
    , address : Address
    , identificationNumber : IdentificationNumber
    , driversLicenseID : DriversLicenseID
    , expirationDate : ExpirationDate
    , birthDate : BirthDate
    }


content : Parser Content
content =
    Content
        `map` ((Parser.symbol '%') *> stateOrProvince)
        `and` city
        `and` name
        `and`
            (address
                <* (Parser.optional (Parser.symbol '^') ' ')
                <* (Parser.many (Parser.symbol ' '))
                <* Parser.symbol '?'
            )
        `and` ((Parser.symbol ';') *> identificationNumber)
        `and` (driversLicenseID <* (Parser.symbol '='))
        `and` expirationDate
        `and`
            (birthDate
                <* (Parser.many digit)
                <* (Parser.optional (Parser.symbol '=') ' ')
                <* Parser.symbol '?'
            )


type alias StateOrProvince =
    String


stateOrProvince : Parser StateOrProvince
stateOrProvince =
    String.fromList `map` (count 2 Parser.Char.upper)


type alias City =
    String


city : Parser City
city =
    let
        allowed =
            Parser.choice
                [ Parser.Char.upper
                , punctuation
                , space
                ]
    in
        String.fromList
            `map`
                Parser.choice
                    [ (count 13 allowed)
                    , (Parser.many allowed) <* Parser.symbol '^'
                    ]


type alias Name =
    { familyName : String
    , givenName : String
    , suffix : String
    }


name : Parser Name
name =
    let
        allowed =
            Parser.choice
                [ Parser.Char.upper
                , punctuation
                , space
                , Parser.symbol '$'
                ]

        stringToName string =
            case (String.split "$" string) of
                f :: g :: s :: [] ->
                    Name f g s

                _ ->
                    Name "" "" ""
    in
        Parser.map stringToName <|
            String.fromList
                `map`
                    Parser.choice
                        [ (count 35 allowed)
                        , (Parser.many allowed) <* Parser.symbol '^'
                        ]


type alias Address =
    List String


address : Parser Address
address =
    let
        allowed =
            Parser.choice
                [ Parser.Char.upper
                , punctuation
                , space
                , digit
                , Parser.symbol '$'
                ]
    in
        Parser.map (String.split "$") <|
            String.fromList
                `map` (Parser.many allowed)


type alias IdentificationNumber =
    String


identificationNumber : Parser IdentificationNumber
identificationNumber =
    String.fromList
        `map` count 6 digit


type alias DriversLicenseID =
    String


driversLicenseID : Parser DriversLicenseID
driversLicenseID =
    String.fromList
        `map` Parser.many digit


type alias ExpirationDate =
    { year : String
    , month : String
    }


expirationDate : Parser ExpirationDate
expirationDate =
    let
        listToDate list =
            ExpirationDate (String.fromList <| List.take 2 list) (String.fromList <| List.drop 2 list)
    in
        listToDate
            `map` count 4 digit


type alias BirthDate =
    { year : String
    , month : String
    , day : String
    }


birthDate : Parser BirthDate
birthDate =
    let
        listToDate list =
            BirthDate
                (String.fromList <| List.take 4 list)
                (String.fromList <| List.take 2 (List.drop 4 list))
                (String.fromList <| List.drop 6 list)
    in
        listToDate
            `map` count 8 digit



-- MAIN


sanitize : String -> String
sanitize string =
    let
        sanitizeDigit d =
            if Char.isDigit d then
                '0'
            else
                d

        sanitizeUpper a =
            if Char.isUpper a then
                'A'
            else
                a

        sanitizeLower a =
            if Char.isLower a then
                'a'
            else
                a
    in
        string
            |> String.map sanitizeDigit
            |> String.map sanitizeUpper
            |> String.map sanitizeLower


main =
    beginnerProgram
        { model = init
        , update = update
        , view = view
        }
