module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Dict
import GitHub
import GitHub.Color as GitHub
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Http
import Json.Decode as Json
import Octicons
import Task
import Url exposing (Url)
import Url.Parser as Url exposing ((</>), (<?>))
import Url.Parser.Query as UrlQuery


main : Program Flag Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type alias Flag =
    { info : Json.Value }


type alias Model =
    { text : String
    , info : List GitHubInfo
    , embed : Bool
    , card : { width : Float, height : Float }
    , url : String
    , key : Nav.Key
    }


type GitHubInfo
    = GitHubUser GitHub.User
    | GitHubRepo GitHub.Repo


type Error
    = NoInput
    | InvalidInput
    | HttpErr Http.Error
    | JsonErr Json.Error


type Msg
    = InputText String
    | GetCardElement (Result Dom.Error Dom.Element)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url


init : Flag -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flag url key =
    let
        info =
            Json.decodeValue GitHub.decodeInfo flag.info
                |> Result.toMaybe
                |> Maybe.map (\{ owner, repos } -> GitHubUser owner :: List.map GitHubRepo repos)
                |> Maybe.withDefault []
    in
    Model "" info False { width = 0, height = 0 } "" key
        |> initModel url


initModel : Url -> Model -> ( Model, Cmd Msg )
initModel url model =
    let
        target =
            { url | path = "" }
                |> Url.parse (Url.top </> Url.top <?> UrlQuery.string "target")
                |> Maybe.withDefault Nothing

        baseUrl =
            Url.toString { url | query = Nothing, fragment = Nothing }
    in
    case ( target, url.fragment ) of
        ( Nothing, Nothing ) ->
            ( { model | url = baseUrl }, Cmd.none )

        ( Just text, _ ) ->
            ( { model | text = text, embed = True }, Cmd.none )

        ( _, Just text ) ->
            ( { model | text = text, url = baseUrl }, getCardElement )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputText txt ->
            ( { model | text = txt }, Cmd.none )

        GetCardElement (Ok dom) ->
            let
                card =
                    { width = dom.element.width, height = dom.element.height }
            in
            ( { model | card = card }, Cmd.none )

        GetCardElement (Err _) ->
            ( model, Cmd.none )

        LinkClicked (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

        UrlChanged url ->
            initModel url model


view : Model -> Browser.Document Msg
view model =
    let
        title =
            "GitHub Card Builder"

        info =
            findInfoByName model.info model.text
    in
    { title = title
    , body =
        if model.embed then
            [ buildCardBy model info ]

        else
            [ Html.div [ Attr.class "p-3" ] [ viewBody title model info ] ]
    }


findInfoByName : List GitHubInfo -> String -> Maybe GitHubInfo
findInfoByName infos name =
    let
        getName info =
            case info of
                GitHubUser user ->
                    user.login

                GitHubRepo repo ->
                    repo.owner ++ "/" ++ repo.name
    in
    List.head <| List.filter (\info -> getName info == name) infos


viewBody : String -> Model -> Maybe GitHubInfo -> Html Msg
viewBody title model singleInfo =
    Html.div [ Attr.class "container" ]
        [ Html.h2 [ Attr.class "pb-3" ] [ Html.text title ]
        , Html.div [ Attr.class "clearfix" ]
            [ Html.a
                [ Attr.class "btn btn-sm btn-with-count"
                , Attr.href ("#" ++ model.text)
                ]
                [ Html.text "Build" ]
            , Html.input
                [ Attr.class "social-count"
                , Event.onInput InputText
                ]
                []
            ]
        , Maybe.map List.singleton singleInfo
            |> Maybe.withDefault model.info
            |> List.map buildCard
            |> List.map List.singleton
            |> List.map (Html.div [ Attr.class "p-5 one-half" ])
            |> Html.div []
        , case singleInfo of
            Nothing ->
                Html.div [] []

            Just info ->
                Html.div []
                    [ Html.h3 [] [ Html.text "Embed:" ]
                    , Html.text (iframe model)
                    ]
        ]


iframe : Model -> String
iframe model =
    String.join " "
        [ "<iframe"
        , "width=\"" ++ String.fromFloat model.card.width ++ "\""
        , "height=\"" ++ String.fromFloat model.card.height ++ "\""
        , "scrolling=\"no\""
        , "frameborder=\"0\""
        , "src=\"" ++ model.url ++ "?target=" ++ model.text ++ "\""
        , "></iframe>"
        ]


buildCard : GitHubInfo -> Html msg
buildCard info =
    case info of
        GitHubUser user ->
            buildUserCard user

        GitHubRepo repo ->
            buildRepoCard repo


buildCardBy : Model -> Maybe GitHubInfo -> Html msg
buildCardBy model minfo =
    case minfo of
        Nothing ->
            Html.div [ Attr.class "flash flash-error" ] [ Html.text ("card is not found: " ++ model.text) ]

        Just info ->
            buildCard info


buildUserCard : GitHub.User -> Html msg
buildUserCard user =
    Html.div [ Attr.id "github-card", Attr.class "Box box-shadow" ]
        [ Html.div [ Attr.class "Box-row" ]
            [ Html.div []
                [ Html.img [ Attr.class "avatar float-left pr-2", Attr.src <| user.avatar ++ "&s=48" ] []
                , Html.div []
                    [ Html.h3 [] [ Html.text <| Maybe.withDefault user.login user.name ]
                    , Html.span [] [ Html.text ("@" ++ user.login) ]
                    ]
                ]
            ]
        , Html.div [ Attr.class "Box-row d-flex text-uppercase" ]
            [ Html.div [ Attr.class "pr-2 border-right" ]
                [ Html.span [] [ Html.text "Repos" ]
                , Html.h4 [] [ Html.text <| String.fromInt user.repoCnt ]
                ]
            , Html.div [ Attr.class "pr-2 pl-2 border-right" ]
                [ Html.span [] [ Html.text "Followers" ]
                , Html.h4 [] [ Html.text <| String.fromInt user.followers ]
                ]
            , Html.div [ Attr.class "pl-2" ]
                [ Html.span [] [ Html.text "Following" ]
                , Html.h4 [] [ Html.text <| String.fromInt user.following ]
                ]
            ]
        , Html.div [ Attr.class "Box-row" ]
            [ Html.a
                [ Attr.class "avatar float-left pr-2", Attr.href user.url, Attr.target "_blank" ]
                [ Octicons.markGithub <| Octicons.size 24 <| Octicons.defaultOptions ]
            , Html.div [] [ Html.text user.bio ]
            ]
        ]


buildRepoCard : GitHub.Repo -> Html msg
buildRepoCard repo =
    let
        border =
            Dict.get repo.language GitHub.colors
                |> Maybe.withDefault Nothing
                |> Maybe.map (String.append "5px solid ")
                |> Maybe.map (Attr.style "border-left")
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
    in
    Html.div ([ Attr.id "github-card", Attr.class "Box box-shadow" ] ++ border)
        [ Html.div [ Attr.class "Box-row" ]
            [ Html.div []
                [ Html.img [ Attr.class "avatar float-left pr-2", Attr.src <| repo.avatar ++ "&s=48" ] []
                , Html.div []
                    [ Html.h3 [] [ Html.text repo.name ]
                    , Html.span [] [ Html.text ("Create by @" ++ repo.owner) ]
                    , Html.div [] [ Html.text repo.description ]
                    ]
                ]
            ]
        , Html.div [ Attr.class "Box-row d-flex text-uppercase" ]
            [ Html.a
                [ Attr.class "avatar float-left pr-2", Attr.href repo.url, Attr.target "_blank" ]
                [ Octicons.markGithub <| Octicons.size 24 <| Octicons.defaultOptions ]
            , Html.div [ Attr.class "pr-2 pl-2" ]
                [ Html.strong [ Attr.class "pr-1" ] [ Html.text <| String.fromInt repo.starCnt ]
                , Html.span [] [ Html.text "Stars" ]
                ]
            , Html.div [ Attr.class "pl-2" ]
                [ Html.strong [ Attr.class "pr-1" ] [ Html.text <| String.fromInt repo.forkCnt ]
                , Html.span [] [ Html.text "Forks" ]
                ]
            ]
        ]


getCardElement : Cmd Msg
getCardElement =
    Task.attempt GetCardElement (Dom.getElement "github-card")
