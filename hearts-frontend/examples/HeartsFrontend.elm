module HeartsFrontend exposing (main)

import Browser
import Html exposing (Html, span, div, button, ul, li, br, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http

import Json.Decode exposing (Decoder)

import Dict
import Random
import Task exposing (Task)

import HeartsGame exposing (..)
import HeartsJson exposing (..)
import Shuffle

type alias Endpoint = String

tableServerEndpointUri : Endpoint
tableServerEndpointUri =
    "http://localhost:8080/command"

playersWithEndpoints : List (Player, Endpoint)
playersWithEndpoints =
    [ (Player "1" "Mike", "http://localhost:8001/event")
    , (Player "2" "Peter", "http://localhost:8002/event")
    , (Player "3" "Nicole", "http://localhost:8003/event")
    , (Player "4" "Annette", "http://localhost:8004/event")
    ]

playerEndpointUris : List Endpoint
playerEndpointUris =
    List.map
        Tuple.second
        playersWithEndpoints

allPlayers : List Player
allPlayers =
    List.map
        Tuple.first
        playersWithEndpoints

randomSeedGenerator : Random.Generator Random.Seed
randomSeedGenerator =
    Random.map
        Random.initialSeed
        (Random.int 0 123456)

findPlayer : PlayerId -> List Player -> Player
findPlayer playerId players =
    let candidates = List.filter (\ player -> player.id == playerId) players
    in
        case List.head candidates of
            Just player -> player
            Nothing -> Debug.todo ("Fatal: no player with ID " ++ playerId ++ " found!")

-- MAIN

main =
  Browser.element
      { init = init
      , update = update
      , view = view
      , subscriptions = subscriptions
      }

-- MODEL

type alias Model =
  { gameState : GameState
  , gameCommands : List GameCommand
  }

init : () -> (Model, Cmd Msg)
init =
    \_ -> ({ gameState = emptyGameState allPlayers
           , gameCommands = []
           }, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- UPDATE

type Msg = Deal Random.Seed
         | ProcessGameCommands
         | GotGameEvents (Result Http.Error (List GameEvent))
         | GotGameCommands (Result Http.Error (List GameCommand))
         | ShuffleCards

postGameCommand : GameCommand -> Cmd Msg
postGameCommand gameCommand =
    Http.post { url = tableServerEndpointUri
              , body = Http.jsonBody (encodeGameCommand gameCommand)
              , expect = Http.expectJson GotGameEvents gameEventsDecoder
              }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      ShuffleCards ->
          (model, Random.generate Deal randomSeedGenerator)
      Deal seed ->
          case Shuffle.distribute (Shuffle.shuffle seed deck) of
              hand1 :: (hand2 :: (hand3 :: (hand4 :: _))) ->
                  ( model
                  , postGameCommand (DealHands (Dict.fromList
                                                    [("1", hand1)
                                                    ,("2", hand2)
                                                    ,("3", hand3)
                                                    ,("4", hand4)])))
              _ -> Debug.todo "Impossible"
      GotGameEvents (Ok gameEvents) ->
          ( {model | gameState = List.foldl processGameEvent model.gameState gameEvents}
          , Task.attempt GotGameCommands (forwardGameEventsSequentially playerEndpointUris gameEvents))
      GotGameEvents (Err err) ->
          Debug.todo (showHttpError err)
      GotGameCommands (Ok commands) ->
          ( {model | gameCommands = model.gameCommands ++ commands}, Cmd.none )
      GotGameCommands (Err err) ->
          Debug.todo (showHttpError err)
      ProcessGameCommands ->
          ( {model | gameCommands = []}
          , Cmd.batch (List.map postGameCommand model.gameCommands))

handleJsonResponse : Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)
        Http.Timeout_ ->
            Err Http.Timeout
        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)
        Http.NetworkError_ ->
            Err Http.NetworkError
        Http.GoodStatus_ _ body ->
            case Json.Decode.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)
                Ok result ->
                    Ok result

forwardGameEventsSequentially : List Endpoint -> List GameEvent -> Task Http.Error (List GameCommand)
forwardGameEventsSequentially endpoints gameEvents =
    let pairs = combine endpoints gameEvents
    in
        Task.map List.concat
            (Task.sequence
            (List.map
                 (\ (endpoint, gameEvent) -> gameEventToTask endpoint gameEvent)
                 pairs))

combine : List a -> List b -> List (a, b)
combine xs ys =
    List.concatMap (\ x -> List.map (Tuple.pair x) ys) xs

gameEventToTask : Endpoint -> GameEvent -> Task Http.Error (List GameCommand)
gameEventToTask endpoint gameEvent =
    Http.task { method = "POST"
              , headers = []
              , url = endpoint
              , body = Http.jsonBody (encodeGameEvent gameEvent)
              , resolver = Http.stringResolver <| handleJsonResponse <| gameCommandsDecoder
              , timeout = Nothing
              }

showHttpError : Http.Error -> String
showHttpError err =
  case err of
    Http.BadUrl s -> "bad url: " ++ s
    Http.Timeout -> "timeout"
    Http.NetworkError -> "network error"
    Http.BadStatus status -> "bad status: " ++ String.fromInt status
    Http.BadBody s -> "bad body: " ++ s

-- VIEW

prettyColoredCard : Card -> Html Msg
prettyColoredCard card =
    span [ style "color" (cardColor card) ]
         [ text (prettyCard card) ]

floatingPrettyColoredCard : Card -> Html Msg
floatingPrettyColoredCard card =
    span [ style "float" "left"
         , style "margin" "5px"
         ]
         [ prettyColoredCard card ]

showHand : Hand -> Html Msg
showHand hand =
    div [] (List.map floatingPrettyColoredCard hand)

showPlayer : Player -> Hand -> Html Msg
showPlayer player hand =
    div [ style "float" "left"
         , style "height" "100px"
         , style "width" "200px"
         ]
         [ text player.name
         , showHand hand
        ]

prettyGameCommand : GameCommand -> Html Msg
prettyGameCommand gameCommand =
    case gameCommand of
        DealHands _ ->
            Debug.todo "should be unnecessary"
        PlayCard player card ->
            let t = "Player " ++ player.name ++ " wants to play "
            in
                div []
                    [ span [ style "float" "left"
                           , style "margin-right" "5px"
                           ]
                           [ text t ]
                    , prettyColoredCard card
                    ]

showGameCommands : List GameCommand -> Html Msg
showGameCommands gameCommands =
     div [ style "height" "100px" ]
         [ text "Pending commands:"
         , ul [] (List.map (\ gameCommand -> li [] [ prettyGameCommand gameCommand ]) gameCommands)
         ]

showTrick : Trick -> Html Msg
showTrick trick =
     div [ style "margin-top" "20px"
         , style "height" "100px"
         ]
         [ text "Current trick:"
         , div [] (List.reverse (List.map
                                    (floatingPrettyColoredCard << Tuple.second)
                                    trick))
         ]

showWinner : Maybe Player -> List Player -> Html Msg
showWinner maybeWinner players =
    case maybeWinner of
        Just winner ->
            let realWinner = findPlayer winner.id players
            in
                div [ style "color" "green"
                    , style "font-weight" "bold"
                    ]
                    [ text (realWinner.name ++ " has won the game!") ]
        Nothing ->
            div [] []

view : Model -> Html Msg
view { gameState, gameCommands } =
    let playersWithHands = List.map (\ player -> case Dict.get player.id gameState.hands of
                                                     Just hand -> (player, hand)
                                                     Nothing -> (player, emptyHand)) gameState.players
    in
    div [style "font-family" "sans-serif"]
        (List.concat [ [ div [ style "width" "100%"
                             , style "background-color" "#ccc"
                             ]
                             [ button [ onClick ShuffleCards
                                      , style "margin" "10px"
                                      ]
                                   [ text "Deal cards" ]
                             , button [ onClick ProcessGameCommands
                                      , style "margin" "5px"
                                      ]
                                   [ text "Process commands" ]] ]
                     , [ br [ style "clear" "both" ] [] ]
                     , List.map (\ (player, hand) -> showPlayer player hand) playersWithHands
                     , [ br [ style "clear" "both" ] [] ]
                     , [ showGameCommands gameCommands ]
                     , [ br [ style "clear" "both" ] [] ]
                     , [ showTrick gameState.trick ]
                     , [ br [ style "clear" "both" ] [] ]
                     , [ showWinner gameState.winner allPlayers ]
                     ])
