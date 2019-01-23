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


type alias ForkEntry =
    { fullName : String
    , updatedAt : String
    }


type ListModel
    = Failure
    | Loading
    | Success (List ForkEntry)


type alias Model =
    { mainRepo : String
    , forks : ListModel
    }


init : () -> ( Model, Cmd Msg )
init _ =
    update (InitData (D.decodeString forkDecoder exampleJSON))
        { forks = Loading
        , mainRepo = "elm/core"
        }



-- UPDATE


type Msg
    = FindRepo
    | ChangeRepo String
    | InitData (Result D.Error (List ForkEntry))
    | GotGif (Result Http.Error (List ForkEntry))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeRepo url ->
            ( { model | mainRepo = url }, Cmd.none )

        FindRepo ->
            ( { model | forks = Loading }, getGithubForks model.mainRepo )

        InitData result ->
            case result of
                Ok urls ->
                    ( { model | forks = Success urls }, Cmd.none )

                Err _ ->
                    ( { model | forks = Failure }, Cmd.none )

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
        [ h2 [] [ text "Github forks" ]
        , viewGif model
        ]


viewGif : Model -> Html Msg
viewGif model =
    let
        repoInput =
            input [ placeholder "Text to reverse", value model.mainRepo, onInput ChangeRepo ] []

        searchButton =
            button [ onClick FindRepo ] [ text "Search" ]
    in
    case model.forks of
        Failure ->
            div []
                [ repoInput
                , searchButton
                , text "I could not load a random cat for some reason. "
                ]

        Loading ->
            div []
                [ repoInput
                , searchButton
                , text "Loading"
                ]

        Success urls ->
            div []
                ([ repoInput
                 , searchButton
                 ]
                    ++ List.map showName urls
                )


showName : ForkEntry -> Html Msg
showName { fullName, updatedAt } =
    p [] [ div [] [ text fullName ], div [] [ text updatedAt ] ]



-- HTTP


getGithubForks : String -> Cmd Msg
getGithubForks url =
    Http.get
        { url = "https://api.github.com/repos/" ++ url ++ "/forks"
        , expect = Http.expectJson GotGif forkDecoder
        }


forkDecoder : D.Decoder (List ForkEntry)
forkDecoder =
    D.list fullNameDecoder


fullNameDecoder : D.Decoder ForkEntry
fullNameDecoder =
    D.map2 ForkEntry
        (D.at [ "full_name" ] D.string)
        (D.at [ "updated_at" ] D.string)



--
--
-- example
--
--


exampleJSON : String
exampleJSON =
    """
[
  {
    "id": 166960143,
    "node_id": "MDEwOlJlcG9zaXRvcnkxNjY5NjAxNDM=",
    "name": "core-2",
    "full_name": "bmorelax/core-2",
    "private": false,
    "owner": {
      "login": "bmorelax",
      "id": 33106667,
      "node_id": "MDQ6VXNlcjMzMTA2NjY3",
      "avatar_url": "https://avatars3.githubusercontent.com/u/33106667?v=4",
      "gravatar_id": "",
      "url": "https://api.github.com/users/bmorelax",
      "html_url": "https://github.com/bmorelax",
      "followers_url": "https://api.github.com/users/bmorelax/followers",
      "following_url": "https://api.github.com/users/bmorelax/following{/other_user}",
      "gists_url": "https://api.github.com/users/bmorelax/gists{/gist_id}",
      "starred_url": "https://api.github.com/users/bmorelax/starred{/owner}{/repo}",
      "subscriptions_url": "https://api.github.com/users/bmorelax/subscriptions",
      "organizations_url": "https://api.github.com/users/bmorelax/orgs",
      "repos_url": "https://api.github.com/users/bmorelax/repos",
      "events_url": "https://api.github.com/users/bmorelax/events{/privacy}",
      "received_events_url": "https://api.github.com/users/bmorelax/received_events",
      "type": "User",
      "site_admin": false
    },
    "html_url": "https://github.com/bmorelax/core-2",
    "description": "Elm's core libraries",
    "fork": true,
    "url": "https://api.github.com/repos/bmorelax/core-2",
    "forks_url": "https://api.github.com/repos/bmorelax/core-2/forks",
    "keys_url": "https://api.github.com/repos/bmorelax/core-2/keys{/key_id}",
    "collaborators_url": "https://api.github.com/repos/bmorelax/core-2/collaborators{/collaborator}",
    "teams_url": "https://api.github.com/repos/bmorelax/core-2/teams",
    "hooks_url": "https://api.github.com/repos/bmorelax/core-2/hooks",
    "issue_events_url": "https://api.github.com/repos/bmorelax/core-2/issues/events{/number}",
    "events_url": "https://api.github.com/repos/bmorelax/core-2/events",
    "assignees_url": "https://api.github.com/repos/bmorelax/core-2/assignees{/user}",
    "branches_url": "https://api.github.com/repos/bmorelax/core-2/branches{/branch}",
    "tags_url": "https://api.github.com/repos/bmorelax/core-2/tags",
    "blobs_url": "https://api.github.com/repos/bmorelax/core-2/git/blobs{/sha}",
    "git_tags_url": "https://api.github.com/repos/bmorelax/core-2/git/tags{/sha}",
    "git_refs_url": "https://api.github.com/repos/bmorelax/core-2/git/refs{/sha}",
    "trees_url": "https://api.github.com/repos/bmorelax/core-2/git/trees{/sha}",
    "statuses_url": "https://api.github.com/repos/bmorelax/core-2/statuses/{sha}",
    "languages_url": "https://api.github.com/repos/bmorelax/core-2/languages",
    "stargazers_url": "https://api.github.com/repos/bmorelax/core-2/stargazers",
    "contributors_url": "https://api.github.com/repos/bmorelax/core-2/contributors",
    "subscribers_url": "https://api.github.com/repos/bmorelax/core-2/subscribers",
    "subscription_url": "https://api.github.com/repos/bmorelax/core-2/subscription",
    "commits_url": "https://api.github.com/repos/bmorelax/core-2/commits{/sha}",
    "git_commits_url": "https://api.github.com/repos/bmorelax/core-2/git/commits{/sha}",
    "comments_url": "https://api.github.com/repos/bmorelax/core-2/comments{/number}",
    "issue_comment_url": "https://api.github.com/repos/bmorelax/core-2/issues/comments{/number}",
    "contents_url": "https://api.github.com/repos/bmorelax/core-2/contents/{+path}",
    "compare_url": "https://api.github.com/repos/bmorelax/core-2/compare/{base}...{head}",
    "merges_url": "https://api.github.com/repos/bmorelax/core-2/merges",
    "archive_url": "https://api.github.com/repos/bmorelax/core-2/{archive_format}{/ref}",
    "downloads_url": "https://api.github.com/repos/bmorelax/core-2/downloads",
    "issues_url": "https://api.github.com/repos/bmorelax/core-2/issues{/number}",
    "pulls_url": "https://api.github.com/repos/bmorelax/core-2/pulls{/number}",
    "milestones_url": "https://api.github.com/repos/bmorelax/core-2/milestones{/number}",
    "notifications_url": "https://api.github.com/repos/bmorelax/core-2/notifications{?since,all,participating}",
    "labels_url": "https://api.github.com/repos/bmorelax/core-2/labels{/name}",
    "releases_url": "https://api.github.com/repos/bmorelax/core-2/releases{/id}",
    "deployments_url": "https://api.github.com/repos/bmorelax/core-2/deployments",
    "created_at": "2019-01-22T08:53:09Z",
    "updated_at": "2019-01-22T08:53:11Z",
    "pushed_at": "2018-12-01T10:56:09Z",
    "git_url": "git://github.com/bmorelax/core-2.git",
    "ssh_url": "git@github.com:bmorelax/core-2.git",
    "clone_url": "https://github.com/bmorelax/core-2.git",
    "svn_url": "https://github.com/bmorelax/core-2",
    "homepage": "http://package.elm-lang.org/packages/elm/core/latest",
    "size": 1759,
    "stargazers_count": 0,
    "watchers_count": 0,
    "language": "Elm",
    "has_issues": false,
    "has_projects": true,
    "has_downloads": true,
    "has_wiki": true,
    "has_pages": false,
    "forks_count": 0,
    "mirror_url": null,
    "archived": false,
    "open_issues_count": 0,
    "license": {
      "key": "other",
      "name": "Other",
      "spdx_id": "NOASSERTION",
      "url": null,
      "node_id": "MDc6TGljZW5zZTA="
    },
    "forks": 0,
    "open_issues": 0,
    "watchers": 0,
    "default_branch": "master"
  },
  {
    "id": 166061526,
    "node_id": "MDEwOlJlcG9zaXRvcnkxNjYwNjE1MjY=",
    "name": "core",
    "full_name": "TD5/core",
    "private": false,
    "owner": {
      "login": "TD5",
      "id": 766653,
      "node_id": "MDQ6VXNlcjc2NjY1Mw==",
      "avatar_url": "https://avatars2.githubusercontent.com/u/766653?v=4",
      "gravatar_id": "",
      "url": "https://api.github.com/users/TD5",
      "html_url": "https://github.com/TD5",
      "followers_url": "https://api.github.com/users/TD5/followers",
      "following_url": "https://api.github.com/users/TD5/following{/other_user}",
      "gists_url": "https://api.github.com/users/TD5/gists{/gist_id}",
      "starred_url": "https://api.github.com/users/TD5/starred{/owner}{/repo}",
      "subscriptions_url": "https://api.github.com/users/TD5/subscriptions",
      "organizations_url": "https://api.github.com/users/TD5/orgs",
      "repos_url": "https://api.github.com/users/TD5/repos",
      "events_url": "https://api.github.com/users/TD5/events{/privacy}",
      "received_events_url": "https://api.github.com/users/TD5/received_events",
      "type": "User",
      "site_admin": false
    },
    "html_url": "https://github.com/TD5/core",
    "description": "Elm's core libraries",
    "fork": true,
    "url": "https://api.github.com/repos/TD5/core",
    "forks_url": "https://api.github.com/repos/TD5/core/forks",
    "keys_url": "https://api.github.com/repos/TD5/core/keys{/key_id}",
    "collaborators_url": "https://api.github.com/repos/TD5/core/collaborators{/collaborator}",
    "teams_url": "https://api.github.com/repos/TD5/core/teams",
    "hooks_url": "https://api.github.com/repos/TD5/core/hooks",
    "issue_events_url": "https://api.github.com/repos/TD5/core/issues/events{/number}",
    "events_url": "https://api.github.com/repos/TD5/core/events",
    "assignees_url": "https://api.github.com/repos/TD5/core/assignees{/user}",
    "branches_url": "https://api.github.com/repos/TD5/core/branches{/branch}",
    "tags_url": "https://api.github.com/repos/TD5/core/tags",
    "blobs_url": "https://api.github.com/repos/TD5/core/git/blobs{/sha}",
    "git_tags_url": "https://api.github.com/repos/TD5/core/git/tags{/sha}",
    "git_refs_url": "https://api.github.com/repos/TD5/core/git/refs{/sha}",
    "trees_url": "https://api.github.com/repos/TD5/core/git/trees{/sha}",
    "statuses_url": "https://api.github.com/repos/TD5/core/statuses/{sha}",
    "languages_url": "https://api.github.com/repos/TD5/core/languages",
    "stargazers_url": "https://api.github.com/repos/TD5/core/stargazers",
    "contributors_url": "https://api.github.com/repos/TD5/core/contributors",
    "subscribers_url": "https://api.github.com/repos/TD5/core/subscribers",
    "subscription_url": "https://api.github.com/repos/TD5/core/subscription",
    "commits_url": "https://api.github.com/repos/TD5/core/commits{/sha}",
    "git_commits_url": "https://api.github.com/repos/TD5/core/git/commits{/sha}",
    "comments_url": "https://api.github.com/repos/TD5/core/comments{/number}",
    "issue_comment_url": "https://api.github.com/repos/TD5/core/issues/comments{/number}",
    "contents_url": "https://api.github.com/repos/TD5/core/contents/{+path}",
    "compare_url": "https://api.github.com/repos/TD5/core/compare/{base}...{head}",
    "merges_url": "https://api.github.com/repos/TD5/core/merges",
    "archive_url": "https://api.github.com/repos/TD5/core/{archive_format}{/ref}",
    "downloads_url": "https://api.github.com/repos/TD5/core/downloads",
    "issues_url": "https://api.github.com/repos/TD5/core/issues{/number}",
    "pulls_url": "https://api.github.com/repos/TD5/core/pulls{/number}",
    "milestones_url": "https://api.github.com/repos/TD5/core/milestones{/number}",
    "notifications_url": "https://api.github.com/repos/TD5/core/notifications{?since,all,participating}",
    "labels_url": "https://api.github.com/repos/TD5/core/labels{/name}",
    "releases_url": "https://api.github.com/repos/TD5/core/releases{/id}",
    "deployments_url": "https://api.github.com/repos/TD5/core/deployments",
    "created_at": "2019-01-16T15:17:44Z",
    "updated_at": "2019-01-16T15:17:47Z",
    "pushed_at": "2018-12-01T10:56:09Z",
    "git_url": "git://github.com/TD5/core.git",
    "ssh_url": "git@github.com:TD5/core.git",
    "clone_url": "https://github.com/TD5/core.git",
    "svn_url": "https://github.com/TD5/core",
    "homepage": "http://package.elm-lang.org/packages/elm/core/latest",
    "size": 1759,
    "stargazers_count": 0,
    "watchers_count": 0,
    "language": "Elm",
    "has_issues": false,
    "has_projects": true,
    "has_downloads": true,
    "has_wiki": true,
    "has_pages": false,
    "forks_count": 0,
    "mirror_url": null,
    "archived": false,
    "open_issues_count": 0,
    "license": {
      "key": "other",
      "name": "Other",
      "spdx_id": "NOASSERTION",
      "url": null,
      "node_id": "MDc6TGljZW5zZTA="
    },
    "forks": 0,
    "open_issues": 0,
    "watchers": 0,
    "default_branch": "master"
  }
]"""
