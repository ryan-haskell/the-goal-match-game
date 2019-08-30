module Main exposing (main)

import Browser
import Color exposing (Color)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import LineChart
import LineChart.Colors as Colors
import LineChart.Dots as Dots
import Random


kids : List Kid
kids =
    [ Andy
    , Ben
    , Chuck
    , Dave
    , Evan
    ]


type alias Flags =
    ()


type alias Model =
    { roll : Maybe Int
    , kid : Kid
    , matches : Matches
    , scores : Scores
    }


type Kid
    = Andy
    | Ben
    | Chuck
    | Dave
    | Evan


type alias Scores =
    { andy : List Float
    , ben : List Float
    , chuck : List Float
    , dave : List Float
    , evan : List Float
    }


type alias KidMap a b =
    { a
        | andy : b
        , ben : b
        , chuck : b
        , dave : b
        , evan : b
    }


initScores : Scores
initScores =
    { andy = []
    , ben = []
    , chuck = []
    , dave = []
    , evan = []
    }


nextKid : Kid -> Kid
nextKid kid =
    case kid of
        Andy ->
            Ben

        Ben ->
            Chuck

        Chuck ->
            Dave

        Dave ->
            Evan

        Evan ->
            Andy


scoreFor : Kid -> Scores -> List Float
scoreFor kid scores =
    accessorFor kid scores


type alias Matches =
    { andy : Int
    , ben : Int
    , chuck : Int
    , dave : Int
    , evan : Int
    }


initMatches : Matches
initMatches =
    { andy = 10000
    , ben = 0
    , chuck = 0
    , dave = 0
    , evan = 0
    }


matchesAvailableFor : Kid -> Matches -> Int
matchesAvailableFor kid matches =
    case kid of
        Andy ->
            matches.andy

        Ben ->
            matches.ben

        Chuck ->
            matches.chuck

        Dave ->
            matches.dave

        Evan ->
            matches.evan


nameFor : Kid -> String
nameFor kid =
    dataFor kid |> .name


accessorFor : Kid -> KidMap a b -> b
accessorFor kid =
    dataFor kid |> .accessor


dataFor :
    Kid
    ->
        { name : String
        , color : Color
        , shape : Dots.Shape
        , accessor : KidMap a b -> b
        }
dataFor kid =
    case kid of
        Andy ->
            { name = "Andy"
            , color = Colors.red
            , shape = Dots.circle
            , accessor = .andy
            }

        Ben ->
            { name = "Ben"
            , color = Colors.gold
            , shape = Dots.triangle
            , accessor = .ben
            }

        Chuck ->
            { name = "Chuck"
            , color = Colors.green
            , shape = Dots.cross
            , accessor = .chuck
            }

        Dave ->
            { name = "Dave"
            , color = Colors.blue
            , shape = Dots.square
            , accessor = .dave
            }

        Evan ->
            { name = "Evan"
            , color = Colors.purple
            , shape = Dots.diamond
            , accessor = .evan
            }


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { roll = Nothing
      , kid = Andy
      , matches = initMatches
      , scores = initScores
      }
    , Cmd.none
    )


type Msg
    = RollDice
    | HandleRoll Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RollDice ->
            ( model
            , Random.generate HandleRoll (Random.int 1 6)
            )

        HandleRoll roll ->
            let
                { scores, matches } =
                    updateScores
                        { kid = model.kid
                        , roll = roll
                        , matches = model.matches
                        , scores = model.scores
                        }
            in
            ( { model
                | roll = Just roll
                , kid = nextKid model.kid
                , scores = scores
                , matches = matches
              }
            , Cmd.none
            )


updateScores : { kid : Kid, roll : Int, matches : Matches, scores : Scores } -> { scores : Scores, matches : Matches }
updateScores { kid, roll, matches, scores } =
    let
        matchesMoved =
            if roll > matchesAvailableFor kid matches then
                matchesAvailableFor kid matches

            else
                roll

        kidScores =
            scoreFor kid scores

        score : Float
        score =
            kidScores
                |> List.drop (List.length kidScores - 1)
                |> List.head
                |> Maybe.withDefault 0
                |> (\lastScore -> lastScore + toFloat matchesMoved - 3.5)
    in
    { scores =
        case kid of
            Andy ->
                { scores | andy = kidScores ++ [ score ] }

            Ben ->
                { scores | ben = kidScores ++ [ score ] }

            Chuck ->
                { scores | chuck = kidScores ++ [ score ] }

            Dave ->
                { scores | dave = kidScores ++ [ score ] }

            Evan ->
                { scores | evan = kidScores ++ [ score ] }
    , matches =
        case kid of
            Andy ->
                { matches
                    | andy = matchesAvailableFor Andy matches - matchesMoved
                    , ben = matchesAvailableFor Ben matches + matchesMoved
                }

            Ben ->
                { matches
                    | ben = matchesAvailableFor Ben matches - matchesMoved
                    , chuck = matchesAvailableFor Chuck matches + matchesMoved
                }

            Chuck ->
                { matches
                    | chuck = matchesAvailableFor Chuck matches - matchesMoved
                    , dave = matchesAvailableFor Dave matches + matchesMoved
                }

            Dave ->
                { matches
                    | dave = matchesAvailableFor Dave matches - matchesMoved
                    , evan = matchesAvailableFor Evan matches + matchesMoved
                }

            Evan ->
                { matches | evan = matchesAvailableFor Evan matches - matchesMoved }
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ header "The goal" "spam the roll button"
        , ul [] (List.map (viewBulletPoint model) kids)
        , p []
            [ button [ Events.onClick RollDice ] [ text "roll" ]
            ]
        , LineChart.view .roll .score (List.map (lineBoy model) kids)
        ]


lineBoy : Model -> Kid -> LineChart.Series Point
lineBoy model kid =
    let
        { name, accessor, color, shape } =
            dataFor kid
    in
    LineChart.line
        color
        shape
        name
        (List.indexedMap (\i s -> Point (toFloat (i + 1)) s) (accessor model.scores))


type alias Point =
    { roll : Float
    , score : Float
    }


viewBulletPoint : Model -> Kid -> Html msg
viewBulletPoint model kid =
    let
        weight : String
        weight =
            if model.kid == kid then
                "red"

            else
                "black"
    in
    li
        [ class "bowl"
        , style "color" weight
        ]
        [ text (nameFor kid)
        , text <| " (" ++ String.fromInt (matchesAvailableFor kid model.matches) ++ " matches)"
        ]


header title subtitle =
    div [ class "app" ]
        [ h1 [] [ text title ]
        , h2 [] [ text subtitle ]
        ]
