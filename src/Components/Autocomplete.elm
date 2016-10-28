module Components.Autocomplete
    exposing
        ( autocomplete
        , Model
        , init
        , ActionType(SetText)
        , update
        )

import String exposing (isEmpty)
import Json.Encode
import Html exposing (Html, text, span, input, a, ul, li)
import Html.Events exposing (onMouseDown, onInput, onFocus, onBlur, onMouseEnter, onMouseLeave)
import Html.Attributes as Attr exposing (style, class, value, property, tabindex)
import Util exposing ((=>), InlineStyle)


type ActionType
    = SetText String
    | EnterPopup
    | LeavePopup
    | Focus
    | Blur
    | Reset


type alias Model =
    { value : String
    , expanded : Bool
    , overPopup : Bool
    }


update : ActionType -> Model -> Model
update action state =
    case action of
        SetText text ->
            { state | value = text }

        EnterPopup ->
            { state | overPopup = True }

        LeavePopup ->
            { state | overPopup = False }

        Focus ->
            { state | expanded = True }

        Blur ->
            { state | expanded = False }

        Reset ->
            { state | value = "" }


init : Model
init =
    { value = ""
    , expanded = False
    , overPopup = False
    }


autocomplete : Model -> List String -> (ActionType -> msg) -> Html msg
autocomplete current collection msg =
    let
        inputStyle =
            if isEmpty current.value then
                css.input ++ [ "width" => "122px" ]
            else
                css.input ++ [ "width" => "100px" ]

        inputProps =
            [ style inputStyle
            , value current.value
            , onInput (\text -> msg (SetText text))
            , onFocus (msg Focus)
            , onBlur (msg Blur)
            ]
    in
        span
            [ style css.wrapper
            ]
            [ input inputProps []
            , (if isEmpty current.value then
                text ""
               else
                a
                    [ style css.button
                    , property "innerHTML" (Json.Encode.string "&times;")
                    , onMouseDown (msg Reset)
                    ]
                    []
              )
            , (if current.expanded && not (List.isEmpty collection) then
                ul
                    [ style css.popupStyle
                    , onMouseEnter (msg EnterPopup)
                    , onMouseLeave (msg LeavePopup)
                    ]
                    (collection
                        |> List.map
                            (\n ->
                                li
                                    [ style css.popupItemStyle
                                    , onMouseDown (msg (SetText n))
                                    ]
                                    [ text n ]
                            )
                    )
               else
                text ""
              )
            ]


css :
    { wrapper : InlineStyle
    , input : InlineStyle
    , button : InlineStyle
    , popupStyle : InlineStyle
    , popupItemStyle : InlineStyle
    }
css =
    { wrapper =
        [ "display" => "inline-flex"
        , "position" => "relative"
        , "z-index" => "2"
        , "outline" => "0"
        , "align-items" => "center"
        , "border" => "1px solid rgba(0, 0, 0, 0.0980392)"
        , "margin-left" => "5px"
        ]
    , input =
        [ "height" => "20px"
        , "border" => "0px"
        , "padding-left" => "5px"
        , "outline" => "0px"
        , "font-size" => "9px"
        , "font-weight" => "normal"
        , "width" => "100px"
        ]
    , button =
        [ "text-decoration" => "none"
        , "color" => "chocolate"
        , "font-weight" => "bold"
        , "border-radius" => "1px"
        , "line-height" => "20px"
        , "height" => "20px"
        , "width" => "20px"
        , "justify-content" => "center"
        , "align-items" => "center"
        , "font-size" => "15px"
        , "margin" => "1px"
        , "display" => "flex"
        , "background-color" => "rgba(100, 0, 0, 0.0470588)"
        , "cursor" => "pointer"
        ]
    , popupStyle =
        [ "position" => "absolute"
        , "top" => "23px"
        , "left" => "-1px"
        , "right" => "-1px"
        , "listStyle" => "none"
        , "background" => "white"
        , "z-index" => "3"
        , "border" => "1px solid rgba(0, 0, 0, .1)"
        , "margin" => "0"
        , "padding" => "0"
        , "borderTop" => "0"
        , "maxHeight" => "200px"
        , "overflowY" => "auto"
        , "fontSize" => "9px"
        , "font-family" => "monospace"
        ]
    , popupItemStyle =
        [ "margin" => "0"
        , "padding" => "5px"
        , "cursor" => "default"
        ]
    }
