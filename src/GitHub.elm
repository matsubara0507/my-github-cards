module GitHub exposing (Info, Repo, User, decodeInfo, getRepo, getUser)

import Http
import Json.Decode as Decode exposing (Decoder)


type alias Info =
    { owner : User
    , repos : List Repo
    }


type alias User =
    { login : String
    , name : Maybe String
    , avatar : String
    , url : String
    , followers : Int
    , following : Int
    , repoCnt : Int
    , bio : String
    }


type alias Repo =
    { name : String
    , owner : String
    , avatar : String
    , url : String
    , description : String
    , starCnt : Int
    , forkCnt : Int
    , language : String
    }


decodeInfo : Decoder Info
decodeInfo =
    Decode.map2 Info
        (Decode.field "owner" decodeUser)
        (Decode.field "repos" <| Decode.list decodeRepo)


decodeUser : Decoder User
decodeUser =
    Decode.map8 User
        (Decode.field "login" Decode.string)
        (Decode.field "name" <| Decode.maybe Decode.string)
        (Decode.field "avatar" Decode.string)
        (Decode.field "url" Decode.string)
        (Decode.field "followers" Decode.int)
        (Decode.field "following" Decode.int)
        (Decode.field "repo_cnt" Decode.int)
        (Decode.field "bio" Decode.string)


decodeRepo : Decoder Repo
decodeRepo =
    Decode.map8 Repo
        (Decode.field "name" Decode.string)
        (Decode.field "owner" Decode.string)
        (Decode.field "avatar" Decode.string)
        (Decode.field "url" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "star_cnt" Decode.int)
        (Decode.field "fork_cnt" Decode.int)
        (Decode.field "language" Decode.string)


getUser : String -> (Result Http.Error User -> msg) -> Cmd msg
getUser owner toMsg =
    Http.get
        { url = String.concat [ baseUrl, "users/", owner ]
        , expect = Http.expectJson toMsg decodeUser
        }


getRepo : String -> String -> (Result Http.Error Repo -> msg) -> Cmd msg
getRepo owner name toMsg =
    Http.get
        { url = String.concat [ baseUrl, "repos/", owner, "/", name ]
        , expect = Http.expectJson toMsg decodeRepo
        }


baseUrl : String
baseUrl =
    "https://api.github.com/"
