port module Main exposing (main)

{-| time to face gravity.
-}

import Audio exposing (Audio, AudioCmd, AudioData)
import Browser
import Browser.Dom
import Browser.Events
import Collage exposing (Collage)
import Collage.Layout
import Collage.Render
import Collage.Text
import Color exposing (Color, rgb, rgb255, rgba)
import Color.Manipulate as Color
import Element as Ui
import Element.Background as Background
import Element.Font as Font
import Element.Input as UiInput
import Html
import Html.Attributes
import Json.Decode
import Json.Encode
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import List exposing (tail)
import List.Extra as List
import List.NonEmpty exposing (NonEmpty)
import Random
import Task
import Time exposing (Posix)
import Xy exposing (Xy, x, xy, y)


main : Program () (Audio.Model Msg Model) (Audio.Msg Msg)
main =
    Audio.documentWithAudio
        { init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        , audioPort = { toJS = audioPortToJS, fromJS = audioPortFromJS }
        , view = viewDocument
        , audio = audio
        }


type alias Model =
    { windowSize : Xy Float
    , pressedKeys : List Key
    , gameStage : GameStage
    , timePlaying : Int -- millis
    , music : Maybe Audio.Source
    }


type GameStage
    = Playing
        { planets : NonEmpty Planet -- the head is the player
        , stars : List Star
        , explosions : List Explosion
        }
    | GameOver


type alias Planet =
    { position : Xy Float
    , v : Xy Float
    , r : Float
    , color : Color
    , whenHit : ActionWhenHit -- not in use
    , tail : List (Xy Float)
    , deadTails : List (List (Xy Float))
    }


type ActionWhenHit
    = Split
    | Join
    | Kill


type alias Star =
    { position : Xy Float
    , r : Float
    }


type alias Explosion =
    { position : Xy Float
    , color : Color
    , r : Float
    }


mapV : (v -> v) -> { r | v : v } -> { r | v : v }
mapV map r =
    { r | v = map r.v }


mapPosition :
    (pos -> pos)
    -> { r | position : pos }
    -> { r | position : pos }
mapPosition map r =
    { r | position = map r.position }


distance :
    { rA | position : Xy Float }
    -> { rB | position : Xy Float }
    -> Float
distance a b =
    difference a.position b.position
        |> length


player : NonEmpty a -> a
player =
    List.NonEmpty.head


mass : { r | r : Float } -> Float
mass { r } =
    r ^ 2.3


massToR : Float -> Float
massToR m =
    m ^ (1 / 2.3)


init : ( Model, Cmd Msg, AudioCmd Msg )
init =
    ( { windowSize = Xy.zero
      , pressedKeys = []
      , gameStage =
            { planets =
                { v = Xy.zero
                , position = Xy.zero
                , r = 2
                , color = rgb 1 1 0
                , whenHit = Join
                , tail = []
                , deadTails = []
                }
                    |> List.NonEmpty.singleton
            , stars = []
            , explosions = []
            }
                |> Playing
      , timePlaying = 0
      , music = Nothing
      }
    , List.repeat 9
        (randomPlanet { awayFrom = Xy.zero, atLeast = 70 }
            |> Random.generate PlanetGenerated
        )
        ++ [ randomStar { awayFrom = Xy.zero, atLeast = 0 }
                |> Random.list 800
                |> Random.generate StarsGenerated
           , Browser.Dom.getViewport
                |> Task.perform
                    (.viewport >> Xy.fromSize >> Resized)
           ]
        |> Cmd.batch
    , Audio.loadAudio SoundLoadingResult
        "https://github.com/lue-bird/time-to-face-gravity/blob/master/music/space%202.wav"
    )


type Msg
    = NewGameClicked
    | Resized (Xy Float)
    | Frame Int
    | KeyMsg Keyboard.Msg
    | PlanetGenerated Planet
    | StarsGenerated (List Star)
    | SoundLoadingResult (Result Audio.LoadError Audio.Source)


update : AudioData -> Msg -> Model -> ( Model, Cmd Msg, AudioCmd Msg )
update _ msg =
    case msg of
        NewGameClicked ->
            \_ -> init

        Resized windowSize ->
            \m ->
                ( { m
                    | windowSize =
                        windowSize
                            |> Xy.map (\c -> c - 3.5)
                  }
                , Cmd.none
                , Audio.cmdNone
                )

        Frame millis ->
            \model ->
                case model.gameStage of
                    Playing playing ->
                        let
                            ( player_, nonPlayerPlanets ) =
                                playing.planets

                            updatePlanet planet =
                                let
                                    cappedV =
                                        planet.v |> lengthAtMost 2
                                in
                                { planet | v = cappedV }
                                    |> mapPosition (Xy.map2 (+) cappedV)

                            arrows =
                                Keyboard.Arrows.arrows model.pressedKeys
                                    |> Xy.fromXY
                                    |> Xy.map toFloat

                            planetsUpdatedPlayer =
                                ( player_
                                    |> mapV
                                        (Xy.map2 (+)
                                            (arrows |> Xy.map ((*) 1.2))
                                        )
                                , nonPlayerPlanets
                                )

                            ( movedPlayer, movedPlanets ) =
                                planetsMoved

                            planetsWithGravity : NonEmpty Planet -> NonEmpty Planet
                            planetsWithGravity planets =
                                planets
                                    |> List.NonEmpty.indexedMap
                                        (\i planet ->
                                            planets
                                                |> List.NonEmpty.toList
                                                |> List.removeAt i
                                                |> List.foldl
                                                    (\other ->
                                                        mapV
                                                            (Xy.map2 (+)
                                                                (Xy.direction
                                                                    (difference planet.position other.position
                                                                        |> Xy.toAngle
                                                                    )
                                                                    |> Xy.map
                                                                        ((*)
                                                                            ((mass other * 0.4)
                                                                                / (distance planet other ^ 1)
                                                                            )
                                                                        )
                                                                    |> lengthAtMost 0.2
                                                                )
                                                            )
                                                    )
                                                    planet
                                        )

                            planetsMoved =
                                planetsUpdatedPlayer
                                    |> planetsWithGravity
                                    |> List.NonEmpty.map updatePlanet

                            updateTail =
                                List.NonEmpty.map
                                    (\planet ->
                                        { planet
                                            | tail =
                                                planet.position
                                                    :: planet.tail
                                                    |> List.take 172
                                        }
                                    )

                            overlap planet other =
                                distance planet other
                                    < max planet.r other.r

                            collide : NonEmpty Planet -> List Planet
                            collide planets =
                                let
                                    planet =
                                        planets |> List.NonEmpty.head
                                in
                                planets
                                    |> List.NonEmpty.filter (overlap planet)
                                    |> Maybe.andThen
                                        (\overlapping ->
                                            if (overlapping |> List.NonEmpty.length) >= 2 then
                                                Just overlapping

                                            else
                                                Nothing
                                        )
                                    |> Maybe.map
                                        (\overlapping ->
                                            let
                                                sum aspect =
                                                    List.map aspect
                                                        >> List.sum

                                                avg aspect list =
                                                    sum aspect list
                                                        / (List.length list
                                                            |> toFloat
                                                          )

                                                componentAvg component =
                                                    (overlapping
                                                        |> List.NonEmpty.map
                                                            (\{ r, color } ->
                                                                color |> Color.toRgba |> component |> (*) r
                                                            )
                                                        |> List.NonEmpty.sum
                                                    )
                                                        / sum .r joiners

                                                biggestR =
                                                    overlapping
                                                        |> List.NonEmpty.map .r
                                                        |> List.NonEmpty.maximum

                                                biggest =
                                                    overlapping
                                                        |> List.NonEmpty.filter
                                                            (.r >> (==) biggestR)
                                                        |> Maybe.map List.NonEmpty.head
                                                        |> Maybe.withDefault planet

                                                joiners =
                                                    overlapping
                                                        |> List.NonEmpty.toList
                                                        |> List.filter (.whenHit >> (==) Join)

                                                splitters =
                                                    overlapping
                                                        |> List.NonEmpty.toList
                                                        |> List.filter (.whenHit >> (==) Split)

                                                splitCount =
                                                    (splitters |> List.length) + 1

                                                splitPlanet { v, position } =
                                                    { position = position
                                                    , color =
                                                        rgb (componentAvg .red)
                                                            (componentAvg .green)
                                                            (componentAvg .blue)
                                                    , v = v
                                                    , r =
                                                        sum mass joiners
                                                            / toFloat splitCount
                                                            |> massToR
                                                    , whenHit = Join
                                                    , tail = biggest.tail
                                                    , deadTails = biggest.deadTails
                                                    }

                                                killerCount =
                                                    overlapping
                                                        |> List.NonEmpty.toList
                                                        |> List.filter (.whenHit >> (==) Kill)
                                                        |> List.length
                                            in
                                            if killerCount > 0 then
                                                []

                                            else if splitCount == 1 then
                                                [ planet ]

                                            else
                                                List.range 0 (splitCount - 1)
                                                    |> List.map
                                                        (\i ->
                                                            splitPlanet
                                                                { v =
                                                                    let
                                                                        splitAngle =
                                                                            ( .v >> Xy.x, .v >> Xy.y )
                                                                                |> Xy.map
                                                                                    (\aspect -> avg aspect splitters)
                                                                                |> Xy.toAngle
                                                                    in
                                                                    Xy.direction
                                                                        (splitAngle
                                                                            + turns (1 / 4)
                                                                            + turns
                                                                                ((toFloat i + 1) / toFloat splitCount)
                                                                        )
                                                                        |> Xy.map ((*) 5.3)
                                                                , position =
                                                                    biggest.position
                                                                        |> Xy.map2 (+)
                                                                            (Xy.direction
                                                                                (turns
                                                                                    ((toFloat i + 1) / toFloat splitCount)
                                                                                )
                                                                                |> Xy.map ((*) (sum .r joiners))
                                                                            )
                                                                }
                                                        )
                                        )
                                    |> Maybe.withDefault [ planet ]
                                    |> (\overlapping ->
                                            planets
                                                |> List.NonEmpty.filter (not << overlap planet)
                                                |> Maybe.map
                                                    (collide
                                                        >> (\planets_ ->
                                                                overlapping ++ planets_
                                                           )
                                                    )
                                                |> Maybe.withDefault overlapping
                                       )

                            collidedPlanets =
                                ( movedPlayer
                                , movedPlanets
                                    |> List.filter
                                        (\planet ->
                                            distance planet movedPlayer < 400
                                        )
                                )
                                    |> updateTail
                                    |> collide

                            overlaps ( planet, others ) =
                                others
                                    |> List.filter (overlap planet)
                                    |> (++)
                                        (case others |> List.filter (not << overlap planet) of
                                            head :: tail ->
                                                overlaps ( head, tail )

                                            [] ->
                                                []
                                        )

                            newExplosions =
                                overlaps planetsMoved
                                    |> List.map
                                        (\{ r, position, color } ->
                                            { r = r, position = position, color = color }
                                        )
                        in
                        ( { model
                            | gameStage =
                                case collidedPlanets of
                                    head :: tail ->
                                        if head.position == movedPlayer.position then
                                            { playing
                                                | planets = ( head, tail )
                                                , explosions =
                                                    playing.explosions
                                                        |> List.map (\ex -> { ex | r = ex.r + 3 })
                                                        |> List.filter (\{ r } -> r < 120)
                                                        |> (++) newExplosions
                                            }
                                                |> Playing

                                        else
                                            GameOver

                                    [] ->
                                        GameOver
                            , timePlaying =
                                model.timePlaying + 1
                          }
                        , case collidedPlanets of
                            player__ :: _ ->
                                if (millis |> modBy 17) == 0 then
                                    randomPlanet
                                        { awayFrom = player__.position
                                        , atLeast = 120
                                        }
                                        |> Random.generate PlanetGenerated

                                else if (millis |> modBy 23) == 0 then
                                    randomStar
                                        { awayFrom = player__.position
                                        , atLeast = 120
                                        }
                                        |> Random.list 540
                                        |> Random.generate StarsGenerated

                                else
                                    Cmd.none

                            [] ->
                                Cmd.none
                        , Audio.cmdNone
                        )

                    GameOver ->
                        ( { model
                            | timePlaying =
                                model.timePlaying + 1
                          }
                        , Cmd.none
                        , Audio.cmdNone
                        )

        KeyMsg keyMsg ->
            \model ->
                ( { model
                    | pressedKeys =
                        Keyboard.update keyMsg model.pressedKeys
                  }
                , Cmd.none
                , Audio.cmdNone
                )

        PlanetGenerated planet ->
            \model ->
                ( { model
                    | gameStage =
                        case model.gameStage of
                            Playing playing ->
                                let
                                    ( player_, tail ) =
                                        playing.planets
                                in
                                { playing
                                    | planets = ( player_, planet :: tail )
                                }
                                    |> Playing

                            GameOver ->
                                GameOver
                  }
                , Cmd.none
                , Audio.cmdNone
                )

        StarsGenerated newStars ->
            \model ->
                ( case model.gameStage of
                    Playing playing ->
                        { model
                            | gameStage =
                                { playing
                                    | stars =
                                        let
                                            player_ =
                                                player playing.planets
                                        in
                                        playing.stars
                                            |> List.filter
                                                (\star -> distance star player_ < 120)
                                            |> (++) newStars
                                }
                                    |> Playing
                        }

                    GameOver ->
                        model
                , Cmd.none
                , Audio.cmdNone
                )

        SoundLoadingResult result ->
            \m ->
                ( { m | music = Result.toMaybe result }
                , Cmd.none
                , Audio.cmdNone
                )


randomPlanet :
    { awayFrom : Xy Float, atLeast : Float }
    -> Random.Generator Planet
randomPlanet { awayFrom, atLeast } =
    Random.map5 Planet
        (Random.map2 xy
            (Random.float atLeast 200 |> randomSign)
            (Random.float atLeast 200 |> randomSign)
            |> Random.map (Xy.map2 (+) awayFrom)
        )
        (Random.map2 xy
            (Random.float -2 2)
            (Random.float -2 2)
        )
        (Random.map (\r -> 2 ^ r)
            (Random.float 0.33 1)
        )
        randomColor
        (Random.weighted
            ( 3, Join )
            [ ( 2, Split )
            , ( 1, Kill )
            ]
        )
        |> randomAndMap (Random.constant [])
        |> randomAndMap (Random.constant [])


randomColor : Random.Generator Color
randomColor =
    let
        component =
            Random.int 0 255
    in
    Random.map3 rgb255 component component component


randomSign : Random.Generator number -> Random.Generator number
randomSign =
    Random.map2 (\f a -> f a)
        (Random.uniform identity [ \x -> -x ])


randomStar :
    { awayFrom : Xy Float, atLeast : Float }
    -> Random.Generator Star
randomStar { awayFrom, atLeast } =
    Random.map2 Star
        (Xy.both
            (Random.float atLeast 200 |> randomSign)
            |> xyRandom
            |> Random.map (Xy.map2 (+) awayFrom)
        )
        (Random.float 0.02 0.44)



-- The ports must have these specific names.


port audioPortToJS : Json.Encode.Value -> Cmd msg


port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg


subscriptions : AudioData -> Model -> Sub Msg
subscriptions _ _ =
    [ Browser.Events.onResize
        (\w h ->
            Resized (( w, h ) |> Xy.map toFloat)
        )
    , Browser.Events.onAnimationFrame
        (Time.posixToMillis >> Frame)
    , Sub.map KeyMsg Keyboard.subscriptions
    ]
        |> Sub.batch


viewDocument : AudioData -> Model -> Browser.Document Msg
viewDocument _ model =
    { title = "time to face gravity."
    , body =
        let
            content =
                case model.gameStage of
                    Playing playStage ->
                        view model playStage
                            |> Collage.Render.svgBox model.windowSize
                            |> Ui.html

                    GameOver ->
                        viewGameOver
        in
        content
            |> Ui.layout
                [ Html.Attributes.style "overflow" "hidden"
                    |> Ui.htmlAttribute
                , Ui.height Ui.fill
                , Ui.width Ui.fill
                , Background.color (Ui.rgb 0 0 0)
                ]
            |> List.singleton
    }


viewGameOver : Ui.Element Msg
viewGameOver =
    [ Ui.text "Game over."
    , UiInput.button []
        { label = Ui.text "Try again!"
        , onPress = Just NewGameClicked
        }
    ]
        |> Ui.column
            [ Font.size 50
            , Font.color (Ui.rgb 1 1 1)
            , Ui.centerX
            , Ui.centerY
            ]


view { windowSize } { stars, explosions, planets } =
    let
        ( player_, planets_ ) =
            planets

        viewTail { tail, color, r } =
            [ Collage.solid (r * 2)
                (color
                    |> Color.fadeOut 0.972
                    |> Collage.uniform
                )
            , Collage.solid 0.07
                (color
                    |> Color.fadeOut 0.45
                    |> Collage.uniform
                )
            ]
                |> List.map
                    (\traceStyle ->
                        tail
                            |> Collage.path
                            |> Collage.traced traceStyle
                    )
                |> Collage.group

        viewPlanet planet =
            let
                orbits =
                    5
            in
            (case planet.whenHit of
                Join ->
                    Collage.circle planet.r
                        |> Collage.filled
                            (planet.color |> Collage.uniform)

                Kill ->
                    Collage.ngon 5 planet.r
                        |> Collage.outlined
                            (Collage.solid (planet.r / 2)
                                (planet.color
                                    |> Color.darken 0.04
                                    |> Collage.uniform
                                )
                            )

                Split ->
                    Collage.ngon 3 planet.r
                        |> Collage.filled
                            (planet.color
                                |> Collage.uniform
                            )
                        |> Collage.rotate
                            ((planet.v |> Xy.toAngle) + turns (1 / 12))
            )
                :: (List.range 1 orbits
                        |> List.map
                            (\orbit ->
                                Collage.circle
                                    (planet.r + toFloat orbit ^ 3 * 0.7)
                                    |> Collage.outlined
                                        (Collage.broken
                                            [ ( planet.r * 2 * pi - 32 |> round, 32 ) ]
                                            2
                                            (Collage.uniform
                                                (rgba 1 1 1 (0.025 - 0.025 * toFloat orbit / toFloat orbits))
                                            )
                                            |> (\style -> { style | cap = Collage.Round })
                                        )
                            )
                   )
                |> Collage.group

        viewStar { position, r } =
            let
                viewTriangle =
                    Collage.ngon 3 r
                        |> Collage.filled
                            (rgba 1 1 1 0.5 |> Collage.uniform)
            in
            [ viewTriangle
            , viewTriangle
                |> Collage.rotate (turns (1 / 6))
            ]
                |> Collage.group
                |> Collage.shift position

        viewExplosion { r, color, position } =
            [ List.range 0 32
                |> List.map
                    (\i ->
                        Xy.direction (turns (toFloat i / 34))
                            |> Xy.map
                                ((*)
                                    (if (i |> modBy 2) == 0 then
                                        0

                                     else
                                        r * 1.7
                                    )
                                )
                    )
                |> Collage.path
                |> Collage.traced
                    (color
                        |> Color.fadeOut (1 - 0.2 / (r ^ 1.2))
                        |> Collage.uniform
                        |> Collage.solid 1
                    )
            , Collage.circle r
                |> Collage.filled
                    (color
                        |> Color.fadeOut (1 - 1 / (r ^ 1.2))
                        |> Collage.uniform
                    )
            ]
                |> Collage.group
                |> Collage.shift position
    in
    [ [ viewPlanet player_
      , viewTail player_
            :: (planets_
                    |> List.map
                        (\planet ->
                            [ viewTail planet
                            , viewPlanet planet
                                |> Collage.shift planet.position
                            ]
                        )
                    |> List.concat
               )
            ++ (stars |> List.map viewStar)
            ++ (explosions |> List.map viewExplosion)
            |> Collage.group
            |> Collage.shift
                (player_.position |> Xy.map (\c -> -c))
      ]
        |> Collage.group
        |> Collage.scale 10
    , Collage.rectangle (x windowSize) (y windowSize)
        |> Collage.filled
            (Collage.uniform (rgb 0 0 0))
    ]
        |> Collage.group


audio : AudioData -> Model -> Audio
audio _ model =
    case model.music of
        Just music ->
            Audio.audio music
                (Time.millisToPosix model.timePlaying)

        Nothing ->
            Audio.silence



-- util


with : second -> first -> ( first, second )
with second =
    \first -> ( first, second )


randomAndMap :
    Random.Generator a
    -> Random.Generator (a -> b)
    -> Random.Generator b
randomAndMap randomAspect =
    Random.map2 (\a f -> f a) randomAspect


difference : Xy number -> Xy number -> Xy number
difference =
    Xy.map2 (\aC bC -> bC - aC)


length : Xy Float -> Float
length ( x, y ) =
    sqrt (x ^ 2 + y ^ 2)


lengthAtMost : Float -> Xy Float -> Xy Float
lengthAtMost maximumLength xy =
    let
        length_ =
            length xy
    in
    if length_ > maximumLength then
        xy |> Xy.map ((*) (maximumLength / length_))

    else
        xy


xyRandom : Xy (Random.Generator coordinate) -> Random.Generator (Xy coordinate)
xyRandom =
    \( x, y ) -> Random.map2 xy x y
