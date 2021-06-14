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
import Element.Border as UiBorder
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
    , timePlayed : Int -- frames
    , music : Maybe (Result Audio.LoadError Audio.Source)
    , planets : NonEmpty Planet -- the head is the player
    , stars : List Star
    , explosions : List Explosion
    }


type GameStage
    = Playing
    | FinalExplosion { r : Float } -- transition to game over
    | GameOver


type alias Planet =
    { isPlayer : PlanetRole
    , position : Xy Float
    , v : Xy Float
    , r : Float
    , color : Color
    , whenHit : ActionWhenHit -- not in use
    , tail : List (Xy Float)
    , deadTails : List (List (Xy Float))
    }


type PlanetRole
    = Player
    | NoPlayer


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
    newGame
        (Audio.loadAudio SoundLoadingResult
            "https://lue-bird.github.io/time-to-face-gravity/"
        )


newGame : AudioCmd Msg -> ( Model, Cmd Msg, AudioCmd Msg )
newGame audioCmd =
    ( { windowSize = Xy.zero
      , pressedKeys = []
      , planets =
            { isPlayer = Player
            , v = Xy.zero
            , position = Xy.zero
            , r = 2.2
            , color = rgb 1 1 0
            , whenHit = Join
            , tail = []
            , deadTails = []
            }
                |> List.NonEmpty.singleton
      , stars = []
      , explosions = []
      , gameStage = Playing
      , timePlayed = 0
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
    , audioCmd
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
            \_ -> newGame Audio.cmdNone

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
                    Playing ->
                        let
                            ( player_, nonPlayerPlanets ) =
                                model.planets

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
                                            (arrows |> Xy.map ((*) 0.84))
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

                                    nonEmptyOverlapping =
                                        List.NonEmpty.fromCons planet
                                            (planets
                                                |> List.NonEmpty.tail
                                                |> List.filter (overlap planet)
                                            )

                                    overlapping =
                                        nonEmptyOverlapping
                                            |> List.NonEmpty.toList

                                    newPlanets =
                                        case overlapping of
                                            _ :: _ :: _ ->
                                                let
                                                    avg aspect list =
                                                        (list |> List.map aspect |> List.sum)
                                                            / (List.length list
                                                                |> toFloat
                                                              )

                                                    componentAvg component =
                                                        (joiners
                                                            |> List.map
                                                                (\{ r, color } ->
                                                                    color |> Color.toRgba |> component |> (*) r
                                                                )
                                                            |> List.sum
                                                        )
                                                            / rSum

                                                    rSum =
                                                        List.map .r joiners |> List.sum

                                                    biggestR =
                                                        joiners
                                                            |> List.map .r
                                                            |> List.maximum
                                                            -- never
                                                            |> Maybe.withDefault planet.r

                                                    biggest =
                                                        overlapping
                                                            |> List.filter
                                                                (.r >> (==) biggestR)
                                                            |> List.head
                                                            |> Maybe.withDefault planet

                                                    joiners =
                                                        overlapping
                                                            |> List.filter (.whenHit >> (==) Join)

                                                    joinerCount =
                                                        joiners |> List.length

                                                    splitters =
                                                        overlapping
                                                            |> List.filter (.whenHit >> (==) Split)

                                                    splitCount =
                                                        (splitters |> List.length) + 1

                                                    newPlanet { v, position } =
                                                        { isPlayer = planet.isPlayer
                                                        , position = position
                                                        , color =
                                                            rgb (componentAvg .red)
                                                                (componentAvg .green)
                                                                (componentAvg .blue)
                                                        , v = v
                                                        , r =
                                                            (List.map mass joiners |> List.sum)
                                                                / toFloat splitCount
                                                                |> massToR
                                                        , whenHit = Join
                                                        , tail = biggest.tail
                                                        , deadTails = biggest.deadTails
                                                        }

                                                    killerCount =
                                                        overlapping
                                                            |> List.filter (.whenHit >> (==) Kill)
                                                            |> List.length
                                                in
                                                if killerCount > 0 then
                                                    []

                                                else if splitCount == 1 then
                                                    newPlanet
                                                        { v =
                                                            ( .v >> Xy.x, .v >> Xy.y )
                                                                |> Xy.map
                                                                    (\aspect ->
                                                                        List.map aspect joiners
                                                                            |> List.sum
                                                                    )
                                                        , position = biggest.position
                                                        }
                                                        :: []

                                                else
                                                    List.range 0 (splitCount - 1)
                                                        |> List.map
                                                            (\i ->
                                                                newPlanet
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
                                                                                    |> Xy.map ((*) rSum)
                                                                                )
                                                                    }
                                                            )

                                            _ ->
                                                [ planet ]
                                in
                                planets
                                    |> List.NonEmpty.filter (not << overlap planet)
                                    |> Maybe.map
                                        (collide
                                            >> (\planets_ ->
                                                    newPlanets ++ planets_
                                               )
                                        )
                                    |> Maybe.withDefault newPlanets

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
                        ( case collidedPlanets of
                            head :: tail ->
                                case head.isPlayer of
                                    Player ->
                                        { model
                                            | planets = ( head, tail )
                                            , explosions =
                                                model.explosions
                                                    |> List.map (\ex -> { ex | r = ex.r + 3 })
                                                    |> List.filter (\{ r } -> r < 120)
                                                    |> (++) newExplosions
                                            , timePlayed =
                                                model.timePlayed + 1
                                        }

                                    NoPlayer ->
                                        { model
                                            | gameStage =
                                                FinalExplosion
                                                    { r = player model.planets |> .r }
                                        }

                            [] ->
                                { model
                                    | gameStage =
                                        FinalExplosion
                                            { r = player model.planets |> .r }
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

                    FinalExplosion explosion ->
                        ( { model
                            | gameStage =
                                if explosion.r < 120 then
                                    FinalExplosion
                                        { explosion | r = explosion.r + 4.4 }

                                else
                                    GameOver
                          }
                        , Cmd.none
                        , Audio.cmdNone
                        )

                    GameOver ->
                        ( model
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
                ( let
                    ( player_, tail ) =
                        model.planets
                  in
                  { model
                    | planets = ( player_, planet :: tail )
                  }
                , Cmd.none
                , Audio.cmdNone
                )

        StarsGenerated newStars ->
            \model ->
                ( { model
                    | stars =
                        let
                            player_ =
                                player model.planets
                        in
                        model.stars
                            |> List.filter
                                (\star -> distance star player_ < 120)
                            |> (++) newStars
                  }
                , Cmd.none
                , Audio.cmdNone
                )

        SoundLoadingResult result ->
            \m ->
                ( { m | music = Just result }
                , Cmd.none
                , Audio.cmdNone
                )


randomPlanet :
    { awayFrom : Xy Float, atLeast : Float }
    -> Random.Generator Planet
randomPlanet { awayFrom, atLeast } =
    Random.map5 (Planet NoPlayer)
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
            Random.int 30 250
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
            viewPlaying =
                view model
                    |> Collage.Render.svgBox model.windowSize
                    |> Ui.html

            content =
                case model.gameStage of
                    Playing ->
                        viewPlaying

                    GameOver ->
                        viewPlaying
                            |> Ui.el
                                [ Ui.inFront
                                    (viewGameOver model
                                        |> Ui.el
                                            [ Ui.width Ui.fill
                                            , Ui.height Ui.fill
                                            , Background.color (Ui.rgba 0 0 0 0.7)
                                            ]
                                    )
                                ]

                    FinalExplosion { r } ->
                        viewPlaying
                            |> Ui.el
                                [ Ui.inFront
                                    (Collage.circle (r * 10)
                                        |> Collage.filled (Color.rgba 0 0 0 0.7 |> Collage.uniform)
                                        |> Collage.Render.svgBox model.windowSize
                                        |> Ui.html
                                        |> Ui.el
                                            [ Ui.width Ui.fill
                                            , Ui.height Ui.fill
                                            , Ui.inFront
                                                (viewGameOver model
                                                    |> Ui.el
                                                        [ Ui.width Ui.fill
                                                        , Ui.height Ui.fill
                                                        ]
                                                )
                                            ]
                                    )
                                ]
                            |> Ui.el
                                [ Ui.width Ui.fill
                                , Ui.height Ui.fill
                                ]
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


viewGameOver : { a | timePlayed : Int } -> Ui.Element Msg
viewGameOver { timePlayed } =
    [ Ui.text
        ("score " ++ (timePlayed // 24 |> String.fromInt))
        |> Ui.el
            [ Font.size 30
            , Ui.alignBottom
            ]
    , UiInput.button
        [ Font.size 33
        , Background.color (Ui.rgba 0 1 1 0.1)
        , UiBorder.rounded 100
        , Ui.padding 20
        ]
        { label = Ui.text "New game"
        , onPress = Just NewGameClicked
        }
    ]
        |> List.map (Ui.el [ Ui.centerX ])
        |> Ui.column
            [ Font.color (Ui.rgb 1 1 1)
            , Ui.centerX
            , Ui.centerY
            , Ui.spacing 10
            ]


view { windowSize, stars, explosions, planets } =
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
audio _ { timePlayed, music } =
    case music of
        Just (Ok source) ->
            Audio.audio source
                (Time.millisToPosix timePlayed)

        Nothing ->
            Audio.silence

        Just (Err error) ->
            Debug.todo (Debug.toString error)



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


xyRandom :
    Xy (Random.Generator coordinate)
    -> Random.Generator (Xy coordinate)
xyRandom =
    \( x, y ) -> Random.map2 xy x y
