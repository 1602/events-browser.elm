module Components.ErrorRecordOverview
    exposing
        ( viewErrorRecord
        , ViewErrorRecordProps
        )

import String exposing (slice)
import Util exposing (..)
import ErrorRecord exposing (..)
import Messages exposing (Msg(..))
import Html.Attributes as Attr exposing (style, property, id, tabindex)
import Html.Events exposing (onClick)
import Html exposing (Html, div, li, a, span, text)
import Json.Encode as Json


type alias ViewErrorRecordProps =
    { isActive : Bool
    , aboutToDelete : Bool
    }


(?) : Bool -> a -> a -> a
(?) condition a b =
    if condition then
        a
    else
        b


viewErrorRecord : ViewErrorRecordProps -> ErrorRecord -> Html Msg
viewErrorRecord { isActive, aboutToDelete } record =
    let
        backgroundColor =
            (?) isActive
                "#CACFD3"
                "rgba(0, 0, 0, 0.05)"

        borderLeft =
            (?)
                (record.kind == "caught-exception")
                "5px solid rgba(0, 0, 0, 0.1)"
                "5px solid crimson"

        blur =
            (?)
                aboutToDelete
                "blur(1px) grayscale(50%) brightness(145%)"
                ""

        msgStyle =
            css.msgBlock
                ++ [ "background-color" => backgroundColor
                   , "border-left" => borderLeft
                   , "-webkit-filter" => blur
                   ]

        appName =
            record.app
                |> String.split "@"
                |> List.head
                |> Maybe.withDefault record.app

        howLongAgo =
            "1 day ago"

        envName =
            slice 0 3 record.env

        occurrences =
            Json.string <| "&times" ++ (toString record.occurrences)
    in
        li [ id record.id, tabindex 1, style css.listItem, onClick (SelectErrorRecord record.id) ]
            [ div [ style msgStyle ]
                [ a
                    [ style css.wrapper ]
                    [ span [ style css.appName ] [ text appName ]
                    , span [ style css.message ] [ text record.message ]
                    ]
                , span
                    [ style css.errorStats ]
                    [ span
                        [ property "innerHTML" occurrences ]
                        []
                    , text " "
                    , span [ style css.envBadge ] [ text envName ]
                    , text " "
                    , span [] [ text howLongAgo ]
                    ]
                ]
            ]



{- <span className="error-info">
   <span>&times;{ occurrences }</span>
   {' '}
   <span
       style={ clickable }
       className={ envBadgeClassName }
       onClick={ onEnvBadgeClick }
   >{ envName }</span>
   {' '}
   <span>{howLongAgo}</span>
   </span>
-}


css :
    { message : InlineStyle
    , listItem : InlineStyle
    , wrapper : InlineStyle
    , appName : InlineStyle
    , msgBlock : InlineStyle
    , errorStats : InlineStyle
    , envBadge : InlineStyle
    }
css =
    { message =
        [ "white-space" => "nowrap"
        , "overflow" => "hidden"
        , "text-overflow" => "ellipsis"
        , "padding" => "0 5px"
        , "display" => "inline-block"
        , "cursor" => "default"
        ]
    , errorStats =
        [ "flex-shrink" => "0"
        , "font-size" => "10px"
        ]
    , envBadge =
        [ "background" => "rgba(0,0,0,0.2)"
        , "display" => "inline-block"
        , "padding" => "2px"
        , "border-radius" => "1px"
        , "font-weight" => "600"
        , "color" => "white"
        , "text-transform" => "uppercase"
        , "font-size" => "9px"
        , "letter-spacing" => "0.05em"
        ]
    , listItem =
        [ "outline" => "0"
        , "position" => "relative"
        , "list-style" => "none"
        , "margin" => "5px 0 5px 5px"
        , "display" => "flex"
        ]
    , wrapper =
        [ "maxWidth" => "calc(100% - 150px)"
        , "display" => "flex"
        , "flex-shrink" => "0"
        , "font-size" => "12px"
        ]
    , appName =
        [ "flex-shrink" => "0"
        , "font-weight" => "bold"
        , "background" => "rgba(136, 83, 38, 0.45)"
        , "text-align" => "center"
        , "color" => "#eee"
        , "padding" => "2px"
        , "border-radius" => "1px"
        , "font-size" => "9px"
        , "text-transform" => "uppercase"
        , "letter-spacing" => "0.05em"
        , "width" => "50px"
        , "white-space" => "nowrap"
        , "overflow" => "hidden"
        , "text-overflow" => "ellipsis"
        ]
    , msgBlock =
        [ "font-family" => "menlo, monospace"
        , "position" => "relative"
        , "display" => "flex"
        , "box-sizing" => "border-box"
        , "justify-content" => "space-between"
        , "align-items" => "center"
        , "border-radius" => "1px"
        , "padding" => "6px"
        , "width" => "100%"
        , "transition" => "all .2s"
        ]
    }
