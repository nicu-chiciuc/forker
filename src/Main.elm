module Main exposing (Msg(..), getGithubForks, init, main, subscriptions, update, view, viewGif)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type ListModel
    = Failure
    | Loading
    | Success (List String)


type alias Model =
    { mainRepo : String
    , forks : ListModel
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { forks = Loading
      , mainRepo = "elm/core"
      }
    , getGithubForks "elm/core"
    )



-- UPDATE


type Msg
    = FindRepo String
    | GotGif (Result Http.Error (List String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FindRepo url ->
            ( { forks = Loading, mainRepo = url }, getGithubForks url )

        GotGif result ->
            case result of
                Ok urls ->
                    ( { model | forks = Success urls }, Cmd.none )

                Err _ ->
                    ( { model | forks = Failure }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Random Cats" ]
        , viewGif model
        ]


viewGif : Model -> Html Msg
viewGif model =
    let
        repoInput =
            input [ placeholder "Text to reverse", value model.mainRepo, onInput FindRepo ] []
    in
    case model.forks of
        Failure ->
            div []
                [ repoInput
                , text "I could not load a random cat for some reason. "
                ]

        Loading ->
            div []
                [ repoInput
                , text "Loading"
                ]

        Success urls ->
            div []
                (repoInput
                    :: List.map showName urls
                )


showName : String -> Html Msg
showName url =
    p [] [ text url ]



-- HTTP


getGithubForks : String -> Cmd Msg
getGithubForks url =
    Http.get
        { url = "https://api.github.com/repos/" ++ url ++ "/forks"
        , expect = Http.expectJson GotGif forkDecoder
        }


forkDecoder : D.Decoder (List String)
forkDecoder =
    D.list fullNameDecoder


fullNameDecoder : D.Decoder String
fullNameDecoder =
    D.field "full_name" D.string
