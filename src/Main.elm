port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (generate)
import Random.Char exposing (english)
import Random.String exposing (string)



-- PORTS


port copyToClipboard : String -> Cmd msg



-- MODEL


type alias Model =
    { players : List Player
    , currentPlayer : Int
    , gameId : Maybe GameId
    , page : Pages
    , problems : List String
    , matrix : List MatrixMark
    }


type Pages
    = Home
    | Lobby
    | Game


type GameId
    = GameId String


type alias Player =
    { name : Username
    , id : Int
    , isReady : Bool
    }


type Username
    = Username String


type MatrixMark
    = X
    | O
    | Empty


init : flags -> ( Model, Cmd msg )
init _ =
    ( { players = [ Player (Username "Test player 2") 2 True ]
      , currentPlayer = 1
      , gameId = Just (GameId "aoisdfhaosifh")
      , page = Home
      , problems = []
      , matrix = List.repeat 9 Empty
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Html Msg
view model =
    case model.page of
        Home ->
            viewHome model

        Lobby ->
            viewLobby model

        Game ->
            viewGame model


viewHome : Model -> Html Msg
viewHome model =
    div [ class "home-container" ]
        [ h1 []
            [ text "Welcome to Lodjuret2001"
            , br [] []
            , text "Tic Tac Toe Game! :)"
            , br [] []
            ]
        , div [ class "home-selection--wrapper" ]
            [ viewCreateLobby model.players
            , lottie "https://lottie.host/d9f018e5-4ea1-4de7-8fe0-c060ff2ced7a/pgUwwhuAOg.json" "false" "300px" "300px"
            , viewJoinLobby model
            ]
        ]


viewCreateLobby : List Player -> Html Msg
viewCreateLobby players =
    let
        problem =
            validateUsername (toPlayerName (player 1 players))

        -- If the String is Empty then its VALID, otherwise it returns a validation message
        uname =
            toPlayerName (player 1 players)
    in
    div
        [ class "home-selection--container" ]
        [ div [ class "playername-container" ] [ h2 [] [ text ("Player 1: " ++ uname) ] ]
        , input [ placeholder "Enter username", class "username-input", onInput (\str -> UpdatePlayerName str 1) ] []
        , if not (String.isEmpty problem) then
            div [ class "button-wrapper" ]
                [ button [ class "round-button", disabled True ] [ text "Create Lobby" ]
                , p [] [ text problem ]
                ]

          else
            div [ class "button-wrapper" ]
                [ button
                    [ class "round-button", onClick CreateLobby ]
                    [ text "Create Lobby" ]
                ]
        ]


viewJoinLobby : Model -> Html Msg
viewJoinLobby model =
    div
        [ class "home-selection--container" ]
        [ div [ class "playername-container" ] [ h2 [] [ text ("Player 2: " ++ toPlayerName (player 2 model.players)) ] ]
        , input [ placeholder "Enter username", class "username-input", onInput (\str -> UpdatePlayerName str 2) ] []
        , input [ placeholder "Enter GameId", class "username-input", onInput UpdateGameId ] []
        , div [ class "button-wrapper" ]
            [ button
                [ class "round-button round-button--green", onClick JoinLobby ]
                [ text "Join Lobby" ]
            ]
        , div [] (List.map viewFormError model.problems)
        ]


viewFormError : String -> Html Msg
viewFormError err =
    p [] [ text err ]


viewLobby : Model -> Html Msg
viewLobby model =
    div [ class "lobby-container" ]
        [ case model.gameId of
            Nothing ->
                div [ class "lobby-no--gameid" ]
                    [ h2 [] [ text "Sorry! I could not find a gameId for this lobby" ]
                    , lottie "https://lottie.host/fb7dbf0d-f9cf-4de7-aaca-1f62a744edd6/iBRkdaF0zO.json" "true" "300px" "300px"
                    , button [ class "round-button", onClick GoToHome ] [ text "Go Home" ]
                    ]

            Just (GameId id) ->
                div []
                    [ viewClipboard id
                    , div [ class "lobby-players--container" ]
                        [ viewPlayer 1 model.players model.currentPlayer
                        , viewPlayer 2 model.players model.currentPlayer
                        , if model.currentPlayer == 1 then
                            button [ class "round-button", onClick StartGame ] [ text "Start Game" ]

                          else
                            p [] []
                        ]
                    , div [ class "lobby-errors" ] (List.map viewFormError model.problems)
                    ]
        ]


viewPlayer : Int -> List Player -> Int -> Html Msg
viewPlayer id players currentPlayer =
    let
        uname =
            if toPlayerName (player id players) /= "" then
                toPlayerName (player id players)

            else if id == 2 then
                "Waiting for Player " ++ String.fromInt id

            else
                "Could not find Player " ++ String.fromInt id ++ " ..."

        isReady =
            case player id players of
                Nothing ->
                    False

                Just p ->
                    p.isReady
    in
    div [ class "player-container" ]
        [ div [ class ("player-icon " ++ "player-icon--" ++ String.fromInt id) ] [ text ("Player " ++ String.fromInt id) ]
        , p [ class "player-name" ] [ text uname ]
        , button
            [ onClick (UpdateIsPlayerReady id)
            , disabled
                (if id == currentPlayer then
                    False

                 else
                    True
                )
            , class
                (if isReady then
                    "ready-button " ++ "is-ready"

                 else
                    "ready-button " ++ "is-not--ready"
                )
            ]
            [ text
                (if isReady then
                    "IS READY!"

                 else
                    "IS NOT READY!"
                )
            ]
        ]


viewClipboard : String -> Html Msg
viewClipboard id =
    div [ class "clipboard-container" ]
        [ h1 [] [ text ("GameId: " ++ id) ]
        , button [ class "copy-button", onClick (Copy id) ] [ text "📋 Copy" ]
        ]


lottie : String -> String -> String -> String -> Html msg
lottie url loop width height =
    node "dotlottie-player"
        [ Html.Attributes.attribute "src" url
        , Html.Attributes.attribute "background" "transparent"
        , Html.Attributes.attribute "speed" "1"
        , Html.Attributes.attribute "direction" "1"
        , Html.Attributes.attribute "playMode" "normal"
        , Html.Attributes.attribute "loop" loop
        , Html.Attributes.attribute "autoplay" ""
        , style "width" width
        , style "height" height
        ]
        []


viewGame : Model -> Html Msg
viewGame model =
    let
        gameId =
            case model.gameId of
                Just (GameId id) ->
                    id

                Nothing ->
                    "GameId not found"

        username =
            toPlayerName (player model.currentPlayer model.players)
    in
    div [ class "game-container" ]
        [ div [ class "game-info" ]
            [ p [] [ text ("GameId: " ++ gameId) ]
            , p [] [ text ("You are Player " ++ String.fromInt model.currentPlayer) ]
            ]
        , div [ class "game-turn" ]
            [ h2 [] [ text ("It is " ++ username ++ " turn to make a move") ]
            ]
        , div [ class "matrix-container" ] (List.indexedMap viewCell model.matrix)
        ]


viewCell : Int -> MatrixMark -> Html Msg
viewCell index mark =
    let
        content =
            case mark of
                Empty ->
                    "Empty"

                X ->
                    "X"

                O ->
                    "O"
    in
    div [ onClick (ClickedCell index), class "matrix-item" ] [ text content ]



-- UPDATE


type Msg
    = Copy String
    | ClickedCell Int
    | CreateLobby
    | GoToHome
    | GotGameId String
    | JoinLobby
    | StartGame
    | UpdateGameId String
    | UpdateIsPlayerReady Int
    | UpdatePlayerName String Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Copy id ->
            ( model, copyToClipboard id )

        ClickedCell index ->
            let
                mark =
                    if model.currentPlayer == 1 then
                        X

                    else
                        O

                updatedTurn =
                    if model.currentPlayer == 1 then
                        2

                    else
                        1

                updatedMatrix =
                    List.indexedMap
                        (\i matrix ->
                            if i == index then
                                mark

                            else
                                matrix
                        )
                        model.matrix
            in
            if List.length (List.filter (\matrix -> matrix == Empty) updatedMatrix) == 0 then
                ( { model | currentPlayer = updatedTurn, matrix = List.repeat 9 Empty }, Cmd.none )

            else
                ( { model | currentPlayer = updatedTurn, matrix = updatedMatrix }, Cmd.none )

        CreateLobby ->
            ( { model | page = Lobby, currentPlayer = 1 }, createGameId )

        GoToHome ->
            ( { model | page = Home }, Cmd.none )

        GotGameId str ->
            ( { model | gameId = Just (GameId str) }, Cmd.none )

        JoinLobby ->
            let
                errors =
                    validateJoinLobby (player 2 model.players) model.gameId
            in
            if List.length errors == 0 then
                ( { model | page = Lobby, currentPlayer = 2, problems = [] }, Cmd.none )

            else
                ( { model | problems = errors }, Cmd.none )

        StartGame ->
            let
                errors =
                    validateStartGame model.players
            in
            if List.length errors == 0 then
                ( { model | page = Game }, Cmd.none )

            else
                ( { model | problems = errors }, Cmd.none )

        UpdateGameId str ->
            let
                gameId =
                    Just (GameId str)
            in
            ( { model | gameId = gameId }, Cmd.none )

        UpdateIsPlayerReady id ->
            let
                updatedPlayers =
                    case player id model.players of
                        Just _ ->
                            -- Update only existing Players
                            List.map
                                (\p ->
                                    if p.id == id then
                                        { p | isReady = not p.isReady }

                                    else
                                        p
                                )
                                model.players

                        Nothing ->
                            model.players
            in
            ( { model | players = updatedPlayers }, Cmd.none )

        UpdatePlayerName str id ->
            let
                uname =
                    Username str

                updatedPlayers =
                    case player id model.players of
                        Nothing ->
                            -- Add Player if not found
                            Player uname id False :: model.players

                        Just _ ->
                            -- Update the existing Player
                            List.map
                                (\p ->
                                    if p.id == id then
                                        { p | name = uname }

                                    else
                                        p
                                )
                                model.players
            in
            ( { model | players = updatedPlayers }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- HELPERS


createGameId : Cmd Msg
createGameId =
    generate GotGameId (string 15 english)


validateGameId : Maybe GameId -> Bool
validateGameId gameId =
    case gameId of
        Nothing ->
            False

        Just (GameId id) ->
            if String.length id > 5 then
                True

            else
                False


validateUsername : String -> String
validateUsername str =
    if String.isEmpty str then
        "Need to enter a username"

    else if String.length str < 3 then
        "Must have atleast 3 characters"

    else
        ""


validateJoinLobby : Maybe Player -> Maybe GameId -> List String
validateJoinLobby maybePlayer gameId =
    let
        validationMessage =
            validateUsername <|
                toPlayerName maybePlayer

        gameIdValid =
            validateGameId gameId
    in
    if not (validationMessage == "") && not gameIdValid then
        [ validationMessage, "Not valid GameId" ]

    else if not (validationMessage == "") then
        [ validationMessage ]

    else if not gameIdValid then
        [ "Not valid GameId" ]

    else
        []


validateStartGame : List Player -> List String
validateStartGame players =
    let
        allPlayersHaveJoined =
            List.length players == 2

        allPlayersReady =
            List.length (List.filter (\p -> p.isReady == False) players) == 0
    in
    if allPlayersReady && allPlayersHaveJoined then
        []

    else if not allPlayersHaveJoined then
        [ "There arent enough players to start the game!" ]

    else
        [ "Not all players are ready!" ]


toPlayerName : Maybe Player -> String
toPlayerName maybePlayer =
    case maybePlayer of
        Nothing ->
            ""

        Just pl ->
            case pl.name of
                Username str ->
                    str


player : Int -> List Player -> Maybe Player
player id players =
    List.head (List.filter (\p -> p.id == id) players)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
