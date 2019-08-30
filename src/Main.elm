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
    , matchesInBowl : MatchesInBowl
    , matchesMoved : MatchesMoved
    }


type Kid
    = Andy
    | Ben
    | Chuck
    | Dave
    | Evan


type alias KidMap a b =
    { a
        | andy : b
        , ben : b
        , chuck : b
        , dave : b
        , evan : b
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

-- MatchesMovedThisTurn


type alias MatchesMoved =
    { andy : List Int
    , ben : List Int
    , chuck : List Int
    , dave : List Int
    , evan : List Int
    }


matchesMovedFor : Kid -> MatchesMoved -> List Int
matchesMovedFor = accessorFor


-- MatchesInBowl


type alias MatchesInBowl =
    { andy : Int
    , ben : Int
    , chuck : Int
    , dave : Int
    , evan : Int
    }


matchesInBowlFor : Kid -> MatchesInBowl -> Int
matchesInBowlFor = accessorFor


nameFor : Kid -> String
nameFor kid =
    dataFor kid |> .name

type alias Accessor a b =
    KidMap a b -> b

accessorFor : Kid -> Accessor a b
accessorFor kid =
    dataFor kid |> .accessor


dataFor :
    Kid
    ->
        { name : String
        , color : Color
        , shape : Dots.Shape
        , accessor : Accessor a b
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
      , matchesInBowl =
           { andy = 10000
           , ben = 0
           , chuck = 0
           , dave = 0
           , evan = 0
           }
      , matchesMoved =
           { andy = []
           , ben = []
           , chuck = []
           , dave = []
           , evan = []
           }
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
                { matchesMoved, matchesInBowl } =
                    updateMatches
                        { roll = roll
                        , model = model
                        }
            in
            ( { model
                | roll = Just roll
                , kid = nextKid model.kid
                , matchesMoved = matchesMoved
                , matchesInBowl = matchesInBowl
              }
            , Cmd.none
            )


updateMatches :
    { roll : Int, model : Model }
    -> { matchesMoved : MatchesMoved, matchesInBowl : MatchesInBowl }
updateMatches { roll, model } =
    let
        kid =
            model.kid
        matchesMoved =
            model.matchesMoved
        matchesMovedForKid =
            matchesMovedFor kid matchesMoved
        matchesInBowl =
            model.matchesInBowl
        matchesInBowlForKid =
            matchesInBowlFor kid matchesInBowl
        matchesMovedThisTurn =
            if roll > matchesInBowlForKid then
                matchesInBowlForKid

            else
                roll

    in
    { matchesMoved =
        case kid of
            Andy ->
                { matchesMoved | andy = matchesMovedForKid ++ [ matchesMovedThisTurn ] }

            Ben ->
                { matchesMoved | ben = matchesMovedForKid ++ [ matchesMovedThisTurn ] }

            Chuck ->
                { matchesMoved | chuck = matchesMovedForKid ++ [ matchesMovedThisTurn ] }

            Dave ->
                { matchesMoved | dave = matchesMovedForKid ++ [ matchesMovedThisTurn ] }

            Evan ->
                { matchesMoved | evan = matchesMovedForKid ++ [ matchesMovedThisTurn ] }
    , matchesInBowl =
        case kid of
            Andy ->
                { matchesInBowl
                    | andy = matchesInBowlFor Andy matchesInBowl - matchesMovedThisTurn
                    , ben = matchesInBowlFor Ben matchesInBowl + matchesMovedThisTurn
                }

            Ben ->
                { matchesInBowl
                    | ben = matchesInBowlFor Ben matchesInBowl - matchesMovedThisTurn
                    , chuck = matchesInBowlFor Chuck matchesInBowl + matchesMovedThisTurn
                }

            Chuck ->
                { matchesInBowl
                    | chuck = matchesInBowlFor Chuck matchesInBowl - matchesMovedThisTurn
                    , dave = matchesInBowlFor Dave matchesInBowl + matchesMovedThisTurn
                }

            Dave ->
                { matchesInBowl
                    | dave = matchesInBowlFor Dave matchesInBowl - matchesMovedThisTurn
                    , evan = matchesInBowlFor Evan matchesInBowl + matchesMovedThisTurn
                }

            Evan ->
                { matchesInBowl | evan = matchesInBowlFor Evan matchesInBowl - matchesMovedThisTurn }
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
        (List.indexedMap (\i s -> Point (toFloat (i + 1)) s) (scores accessor model))


scores : (Accessor MatchesMoved (List Int)) -> Model -> List Float
scores accessor model =
    model.matchesMoved -- MatchesMoved
    |> accessor -- List Int
    |> List.map (\i -> toFloat i - 3.5) -- List Float
    |> List.foldl scoreReducer (0, []) -- (Float, List Float)
    |> Tuple.second


scoreReducer : Float -> (Float, List Float) -> (Float, List Float)
scoreReducer scoreThisTurn (cumulativeScore, scores_) =
    (cumulativeScore + scoreThisTurn, scores_ ++ [cumulativeScore + scoreThisTurn])

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
        , text <| " (" ++ String.fromInt (matchesInBowlFor kid model.matchesInBowl) ++ " matches)"
        ]


header title subtitle =
    div [ class "app" ]
        [ h1 [] [ text title ]
        , h2 [] [ text subtitle ]
        ]
