module BeatCount exposing (..)

import Browser
import Html.Events.Extra.Mouse as Mouse
import Round
import Task
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, class)
import Time

type alias DelayMillis = Int

type alias Model =
  { beatDelays: List DelayMillis
  , firstLastBeats: Maybe (Time.Posix, Time.Posix)
  }

addBeat : Time.Posix -> Model -> Model
addBeat t model =
  case model.firstLastBeats of
    Nothing -> Model [] (Just (t, t))
    (Just (t0, t1)) ->
      let
        delay = (Time.posixToMillis t) - (Time.posixToMillis t1)
      in Model (delay :: model.beatDelays) (Just (t0, t))

emptyModel : Model
emptyModel = Model [] Nothing

main =
  Browser.element {
    init = init,
    subscriptions = subscriptions,
    update = update,
    view = view
  }

init : () -> (Model, Cmd Msg)
init _ = (emptyModel, Cmd.none)

subscriptions model = Sub.none

type Msg =
    AddBeatAtCurrentTime
  | AddBeatAt Time.Posix
  | ResetBeats

numberOfBeats : Model -> Int
numberOfBeats model =
  case model.beatDelays of
    [] -> case model.firstLastBeats of
            Nothing -> 0
            Just _ -> 1
    xs -> 1 + (List.length xs)

periodDurationMillis : Model -> Maybe Int
periodDurationMillis model =
  case model.firstLastBeats of
    Nothing -> Nothing
    Just (t0, t1) -> Just ((Time.posixToMillis t1) - (Time.posixToMillis t0))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddBeatAtCurrentTime -> (model, Task.perform AddBeatAt Time.now)
    AddBeatAt t -> (addBeat t model, Cmd.none)
    ResetBeats -> (emptyModel, Cmd.none)

getLowHighHz : Model -> Maybe (Float, Float)
getLowHighHz model =
  let
      durMil = periodDurationMillis model
      n = toFloat (numberOfBeats model)
  in case (durMil, n) of
    (Nothing, _) -> Nothing
    (Just 0, _) -> Nothing
    (Just dm, x) ->
        let
          hi = x * 1000.0 / (toFloat dm)
          lo = (x - 1) * 1000.0 / (toFloat dm)
        in Just (lo, hi)

getLowHz : Model -> Maybe Float
getLowHz model =
  case (getLowHighHz model) of
    Nothing -> Nothing
    Just (lo, _) -> Just lo

getHighHz : Model -> Maybe Float
getHighHz model =
  case (getLowHighHz model) of
    Nothing -> Nothing
    Just (_, hi) -> Just hi

hzToBPM : Float -> Float
hzToBPM x = 60 * x

oddEvenString : Int -> String -> String -> String
oddEvenString n a b =
  case (modBy 2 n) of
    0 -> a
    _ -> b

hzToRoundedBPM : Float -> String
hzToRoundedBPM z = Round.round 1 (60 * z)

formatHzRange : Maybe (Float, Float) -> String
formatHzRange m = case m of
  Nothing -> "?"
  Just (lo, hi) -> String.concat
    [ (hzToRoundedBPM lo)
    , "-"
    , (hzToRoundedBPM hi)
    , " bpm"
    ]

formatBeatCount : Int -> String
formatBeatCount n = case n of
  0 -> "Waiting"
  1 -> "Starting"
  _ -> (String.fromInt n) ++ " beats"

view model =
  div [class "card"]
    [ div [ class "card-header" ]
          [ div [ class "card-header-title" ]
                [ text "Beat counter" ]
          ] 
    , div [ class "card-content"
          , Mouse.onDown (\_ -> AddBeatAtCurrentTime)
          , class "columns"
          ]
          [ div [class "column", style "font-size" "300%"]
                [ text (formatBeatCount (numberOfBeats model)) ]
          , div [class "column", style "font-size" "300%"]
                [ text (formatHzRange (getLowHighHz model))]
          ]
    , div [ class "card-footer"
          , class "is-grouped"
          ]
          [ button [ onClick AddBeatAtCurrentTime 
                   , class "button"
                   , class "is-primary"
                   , class "is-large"
                   ]
                   [ text "Beat" ]
          , button [ onClick ResetBeats 
                   , class "button"
                   , class "is-danger"
                   , class "is-large"
                   ]
                   [ text "Reset" ]
          ]
    ]

