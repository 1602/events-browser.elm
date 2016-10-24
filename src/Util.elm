module Util exposing (..)

type alias InlineStyle =
    List ( String, String )

(=>) : a -> b -> ( a, b )
(=>) a b =
    ( a, b )
