port module Main exposing (..)

import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)
import Test exposing (..)
import Expect


type alias Rec =
    { nested : Sub
    }


type alias Sub =
    { foo : String
    , moo : String
    }


type HW
    = Hello
    | World


rec : Rec
rec =
    { nested = { foo = "", moo = "" } }

func : HW -> String -> Rec
func kind upd =
    let
        nested =
            case kind of
                Hello ->
                    { nested | foo = upd }

                World ->
                    { nested | moo = upd }
    in
        { rec | nested = nested }


tests : Test
tests =
    describe "Bug"
        [ test "works" <|
            \() ->
                Expect.equal { nested = { foo = "baz", moo = "" } }
                    (func Hello "baz")
        , test "another" <|
            \() ->
                Expect.equal { nested = { foo = "", moo = "bar" } }
                    (func World "bar")
        ]


main : Program Value
main =
    run emit tests


port emit : ( String, Value ) -> Cmd msg
