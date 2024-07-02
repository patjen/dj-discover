port module Main exposing (..)


import Browser
import Browser.Navigation as Nav
import Bytes.Encode as Bytes
--import Element exposing (..)
--import Element.Background as Background
--import Element.Border as Border
--import Element.Font as Font
--import Element.Input as Input
import Html exposing (..)
import Html.Attributes exposing (name, disabled, style, href, type_, src, usemap, shape, coords, title, id, checked, attribute, class)
import Html.Events exposing (onClick, onInput)
import Svg exposing (svg, rect, circle, polygon, line, image)
import Svg.Attributes as SvgA
import Svg.Events
import Svg exposing (Svg)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import OAuth
import OAuth.AuthorizationCode.PKCE as PKCE
import Random
import Random.List
import Url exposing (Protocol(..), Url)
import Url.Parser exposing ((<?>))
import List exposing (sort)

import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Debug exposing (toString)


main : Program ( List Int, Encode.Value ) Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> randomBytes GotRandomBytes
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


port setStorage : Encode.Value -> Cmd a


port genRandomBytes : () -> Cmd msg


port randomBytes : (List Int -> msg) -> Sub msg


type alias Model =
    { authToken : String
    , randomList : List Int
    , key : Nav.Key
    , url : Url.Url
    , username : String
    , picture : String
    , playlists : List Playlist
    , errorState : Maybe Msg
    , trackFeatures : List TrackFeatures
    , currentPlaylist : Playlist
    , chartHovering : List (CI.Many ChartDatum CI.Any)
    }

-- MARK: Types
type alias Playlist =
    { songs : List Song
    , name : String
    , id : String
    , length : Int
    , snapshot : String
    , imageUrl : String
    }


type alias Song =
    { name : String
    , id : String
    , artists : List String
    , imageUrl : String
    , danceability : Float
    , energy : Float
    , key : Int
    , tempo : Float
    }

type alias TrackFeatures =
    { id : String
    , danceability : Float
    , energy : Float
    , key : Int
    , tempo : Float
    }

type alias ChartDatum =
    { index : Float
    , name : String
    , tempo : Float
    , energy : Float
    , danceability : Float
    }

type Msg
    = GotAccessToken (Result Http.Error PKCE.AuthenticationSuccess)
    | NoOp
    | Error String
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotRandomBytes (List Int)
    | GotUser (Result Http.Error ( String, String ))
    | GotPlaylists (Result Http.Error ( List Playlist, Maybe String ))
    | ClickedShuffle Playlist
    | GotSongs Playlist (Result Http.Error ( List Song, Maybe String ))
    | ShuffledSongs Playlist
    | AddedToQueue Int (List Song)
    | GotTrackFeatures (Result Http.Error TrackFeatures)
    | ClickedPlaylist Playlist
    | ClickedResetCurrentPlaylist
    | ClickedReloadLibrary
    | ClickedSortByDancability
    | ClickedSortByEnergy
    | ClickedSortByTempo
    | ClickedSortByKey
    | ClickedReverseOrder
    | ClickedFilterByDancability Float Float
    | ClickedFilterByEnergy Float Float
    | ClickedFilterByTempo Float Float
    | ClickedFilterByKey Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool
    | OnChartHover (List (CI.Many ChartDatum CI.Any))



-- CONSTANTS
homeUrl : Url.Url
homeUrl =
    { defaultHttpsUrl | host = "patjen.github.io/dj-discover" }
    --{ defaultHttpsUrl | protocol = Http, host = "localhost:8000/index.html" }


apiUrl : Url.Url
apiUrl =
    { defaultHttpsUrl | host = "api.spotify.com/v1" }


clientId : String
clientId =
    "c26f02d5c1d64078a070b5f2b266ca84"





-- INIT


init : ( List Int, Encode.Value ) -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init ( randomList, playlists ) url key =
    let
        p =
            Decode.decodeValue storageDecoder playlists |> Result.withDefault []

        model =
            { authToken = ""
            , randomList = randomList
            , key = key
            , url = url
            , username = ""
            , picture = ""
            , playlists = p
            , errorState = Nothing
            , trackFeatures = []
            , currentPlaylist = Playlist [] "none" "" 0 "" ""
            , chartHovering = []
            }
    in
    case PKCE.parseCode url of
        PKCE.Success { code } ->
            case getCodeVerifier randomList of
                Just c ->
                    ( model, getAuthToken code c )

                Nothing ->
                    update (Error "After recieving an access code a codeVerifier could not be constructed") model

        _ ->
            ( model, Cmd.none )
    --( model, Cmd.none )



-- AUTHENTICATION


getAuthUrl : PKCE.CodeVerifier -> Url
getAuthUrl codeVerifier =
    PKCE.makeAuthorizationUrl
        { clientId = clientId
        , url = { defaultHttpsUrl | host = "accounts.spotify.com", path = "/authorize" }
        , redirectUri = homeUrl
        , scope = [ "playlist-read-private", "user-modify-playback-state" ]
        , state = Nothing
        , codeChallenge = PKCE.mkCodeChallenge codeVerifier
        }


getCodeVerifier : List Int -> Maybe PKCE.CodeVerifier
getCodeVerifier x =
    List.map Bytes.unsignedInt8 x
        |> Bytes.sequence
        |> Bytes.encode
        |> PKCE.codeVerifierFromBytes


getAuthToken : PKCE.AuthorizationCode -> PKCE.CodeVerifier -> Cmd Msg
getAuthToken code verifier =
    Http.request <|
        PKCE.makeTokenRequest GotAccessToken
            { credentials =
                { clientId = clientId
                , secret = Nothing
                }
            , code = code
            , url = { defaultHttpsUrl | host = "accounts.spotify.com", path = "/api/token" }
            , redirectUri = homeUrl
            , codeVerifier = verifier
            }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let 
                updatedModel = { model | url = url }
                noPlaylist = Playlist [] "none" "" 0 "" ""
            in
                case (url.path, url.fragment) of
                    ("/login", _) ->
                        case PKCE.parseCode url of
                            PKCE.Success { code } ->
                                case getCodeVerifier model.randomList of
                                    Just c ->
                                        ( updatedModel, getAuthToken code c )

                                    Nothing ->
                                        ( updatedModel, Cmd.none )

                            _ ->
                                ( updatedModel, genRandomBytes ())

                    (_ , Nothing) ->
                        ( { model | url = url, currentPlaylist = noPlaylist }, Cmd.none)

                    (_, f) ->
                        let 
                            selectedPlaylist = Maybe.withDefault noPlaylist (List.head 
                                (List.filter (\x -> x.id == (Maybe.withDefault "" f)) model.playlists))
                            
                            cmdLoadSongs = 
                                case selectedPlaylist.songs of
                                    [] ->
                                        Cmd.batch
                                            [ getTracks selectedPlaylist model.authToken
                                            , getTrackFeaturesForPlaylist selectedPlaylist model.authToken
                                            ]
                                    _ ->
                                        Cmd.none
                        in
                            ( { model | url = url, currentPlaylist = selectedPlaylist }, cmdLoadSongs )
            
            
        
        GotRandomBytes randomList ->
            case getCodeVerifier randomList of
                Nothing ->
                    update (Error "A codeVerifier could not be constructed") model

                Just c ->
                    ( model, getAuthUrl c |> Url.toString |> Nav.load )

        GotAccessToken (Ok a) ->
            let
                tok =
                    OAuth.tokenToString a.token
            in
            ( { model | authToken = tok }
            , Cmd.batch
                [ Nav.replaceUrl model.key (Url.toString homeUrl)
                , getUser tok
                , get (getUrlFromPath "/me/playlists") (Http.expectJson GotPlaylists playlistsDecoder) tok
                ]
            )

        GotUser (Ok ( name, image )) ->
            ( { model | username = name, picture = image }, Cmd.none )

        GotPlaylists (Ok ( playlists, nextUrl )) ->
            case nextUrl of
                Nothing ->
                    ( { model | playlists = keepUnchanged model.playlists playlists }, Cmd.none )

                Just url ->
                    ( { model | playlists = model.playlists ++ playlists }
                    , get url (Http.expectJson GotPlaylists playlistsDecoder) model.authToken
                    )

        ClickedShuffle p ->
            case p.songs of
                [] ->
                    ( { model | currentPlaylist = p },
                    Cmd.batch
                    [ getTracks p model.authToken
                    , getTrackFeaturesForPlaylist p model.authToken
                    ])

                _ ->
                    ( { model | currentPlaylist = p } , Cmd.none )

        ClickedReloadLibrary ->
            -- let newModel = { model | playlists = List.map (\x -> if x.id == model.currentPlaylist.id then { x | songs = [] } else x) model.playlists }
            -- in
            -- ( newModel, Cmd.batch
            --     [ get (getUrlFromPath "/me/playlists") (Http.expectJson GotPlaylists playlistsDecoder) model.authToken
            --     --, getTracks model.currentPlaylist model.authToken
            --     --, getTrackFeaturesForPlaylist model.currentPlaylist model.authToken
            --     ]
            -- )
            ( { model | playlists = [] }, Cmd.batch 
                [ setStorage (storageEncoder [])
                , get (getUrlFromPath "/me/playlists") (Http.expectJson GotPlaylists playlistsDecoder) model.authToken] )

        ClickedResetCurrentPlaylist ->
            let 
                selectedPlaylist = Maybe.withDefault (Playlist [] "none" "" 0 "" "") (List.head 
                    (List.filter (\x -> x.id == model.currentPlaylist.id) model.playlists))
            in
            ( {model | currentPlaylist = selectedPlaylist}, Cmd.none )
        
        ClickedSortByDancability ->
            let
                old_playlist = model.currentPlaylist
                new_playlist = { old_playlist | songs = List.sortBy .danceability old_playlist.songs }
                sortedSongs =
                    List.sortBy .name old_playlist.songs
                        |> List.reverse  -- Remove this line if you want ascending order
            in
                ( { model | currentPlaylist = new_playlist } , Cmd.none )
        
        ClickedFilterByTempo min max ->
            let
                old_playlist = model.currentPlaylist
                new_playlist = { old_playlist | songs = List.filter (\song -> song.tempo >= min && song.tempo <= max) old_playlist.songs }
            in
                ( { model | currentPlaylist = new_playlist } , Cmd.none )

        GotSongs p (Ok ( songs, nextQuery )) ->
            let
                newP =
                    { p | songs = p.songs ++ songs }

                m =
                    { model | playlists = newP :: List.filter (\x -> newP.id /= x.id) model.playlists,
                              currentPlaylist = newP}
            in
            ( m
            , case nextQuery of
                Nothing ->
                    Cmd.batch
                        [ getTrackFeaturesForPlaylist newP model.authToken
                        , setStorage (storageEncoder m.playlists)
                        ]

                Just url ->
                    get url (Http.expectJson (GotSongs newP) songsDecoder) model.authToken
            )

        GotTrackFeatures (Ok f) ->
            let
                newP =
                    case (List.head model.playlists) of
                        Nothing ->
                            Playlist [] "No playlist selected" "" 0 "" ""

                        Just p ->
                            { p | songs = List.map (\song ->
                                if song.id == f.id 
                                    then { song | 
                                          danceability = f.danceability
                                        , energy = f.energy
                                        , key = f.key
                                        , tempo = f.tempo}
                                    else song) 
                             p.songs
                            }
        
                m =
                    { model | playlists = newP :: List.filter (\x -> newP.id /= x.id) model.playlists,
                              currentPlaylist = newP }
            in
            (m, setStorage (storageEncoder m.playlists))
            
        OnChartHover hovering ->
            ( { model | chartHovering = hovering }, Cmd.none )    

        AddedToQueue count songs ->
            case songs of
                x :: xs ->
                    if count == List.length songs || count == 100 then
                        ( model, Cmd.none )

                    else
                        ( model
                        , post
                            (getUrlFromPath ("/me/player/queue?uri=spotify:track:" ++ x.id))
                            model.authToken
                            (\_ -> AddedToQueue (count + 1) (xs ++ [ x ]))
                        )

                [] ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        _ ->
        --    ( { model | errorState = Just msg }, Cmd.none )
            ( model, Cmd.none )


enqueuePlaylist : Playlist -> Cmd Msg
enqueuePlaylist p =
    Cmd.none
    --Random.generate (AddedToQueue 0) (Random.List.shuffle p.songs)

sortCurrentPlaylistByDancability : Model -> ( Model, Cmd Msg)
sortCurrentPlaylistByDancability model =
     let
        old_playlist = model.currentPlaylist
        new_playlist = { old_playlist | songs = List.sortBy .danceability old_playlist.songs }

    in
    ( { model | currentPlaylist = new_playlist } , Cmd.none )


getTracks : Playlist -> String -> Cmd Msg
getTracks p token =
    get (getUrlFromPath <| "/playlists/" ++ p.id ++ "/tracks") (Http.expectJson (GotSongs p) songsDecoder) token

-- clearSongsOfPlaylist : Playlist -> Model -> Model Cmd Msg
-- clearSongsOfPlaylist p m =
--     let newModel = { m | playlists = List.map (\x -> if x.id == p.id then { x | songs = [] } else x) m.playlists }
--     in
--     ( newModel, setStorage (storageEncoder newModel.playlists))

getTrackFeatures : (Maybe Song) -> String -> Cmd Msg
getTrackFeatures song token =
    case song of
        Nothing ->
            Cmd.none

        Just s ->
            get (getUrlFromPath <| "/audio-features/" ++ s.id) (Http.expectJson GotTrackFeatures trackFeaturesDecoder) token

getTrackFeaturesForPlaylist : Playlist -> String -> Cmd Msg
getTrackFeaturesForPlaylist p token =
    case p.songs of
        [] ->
            Cmd.none

        _ ->
            List.map (\song -> getTrackFeatures (Just song) token) p.songs
                |> Cmd.batch

keepUnchanged : List Playlist -> List Playlist -> List Playlist
keepUnchanged old new =
    let
        replace p =
            case List.filter (\x -> x.id == p.id) new of
                [] ->
                    p

                x :: _ ->
                    if x.snapshot == p.snapshot then
                        x

                    else
                        p
    in
    List.map replace old


getUser : String -> Cmd Msg
getUser =
    get (getUrlFromPath "/me") <|
        Http.expectJson GotUser <|
            Decode.map2 (\x y -> ( x, y ))
                (Decode.field "id" Decode.string)
                (Decode.at [ "images" ] (Decode.index 0 (Decode.field "url" Decode.string)))





-- MARK: VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "DJ Discover"
    , body =
        [ viewNavbar model
        , case model.currentPlaylist.id of
            "" ->
                div [ class "pt-6 mt-5 ml-6 mr-6" ] 
                    [ viewLibrary model
                    --, viewDebug model
                    ]
            _ ->
                div [ class "pt-6 mt-5 ml-6 mr-6" ]
                    [ viewAnalysis model
                    --, viewDebug model
                    ]

        ]
    }
viewNavbar : Model -> Html Msg
viewNavbar model 
    = nav [ attribute "aria-label" "main navigation", class "navbar is-fixed-top pl-6 pr-6", attribute "role" "navigation" ]
        [ div [ class "navbar-brand" ]
            [ a [ class "navbar-item", href "" ]
            [ text "DJ Discover" ]
            ]
        , div [ class "navbar-menu" ]
            [ div [ class "navbar-start" ]
            [ a [ class "navbar-item", href "" ]
                [ text "Playlists" ]
            , a [ class "navbar-item" ]
                [ text "Reset" ]
            ]
            , div [ class "navbar-end" ] [ viewNavbarUser model ]
            ]
        ]

viewNavbarUser: Model -> Html Msg
viewNavbarUser model =
    case isAuthenticated model of
        True ->
            a [ class "navbar-item", href "/"]
                [ span [ class "mr-2" ] [ figure [ class "image is-24x24" ]
                [ img [ class "is-rounded", src model.picture ] [] ]]
                , span [] [text model.username]
                ]

        False ->
            a [ class "navbar-item", href "/login" ]
                [ span [ class "icon mr-1" ] [ i [ class "fa-solid fa-right-to-bracket" ] [] ]
                , span [] [text "Authenticate"]
                ]

        

viewAnalysis : Model -> Html Msg
viewAnalysis m =
    let 
        p = m.currentPlaylist
        viewAnalysisBody =
            case p.songs of
                [] ->
                    div [ class "box is-three-quarters-desktop is-info" ]
                        [ h1 [ class "title is-6" ] [ text "No song data available" ]
                        , h2 [ class "subtitle is-6" ] [ text "No song data was found in local storage. Authenticate with your Spotify account to load the info from Spotify." ]
                        ]

                _ ->
                    div [ class "container is-three-quarters-desktop" ]
                        [ viewAnalysisTable p
                        , div [ class "mb-6" ] []
                        , viewAnalysisChart m
                        ]
    in
    div [ class "container is-three-quarters-desktop"]
        [ div [ class "media" ]
            [ div [ class "media-left" ]
                [ figure [ class "image is-128x128" ]
                    [ img [ src p.imageUrl ] [] ]
                ]
            , div [ class "media-content" ]
                [ h1 [ class "title" ] [ text p.name]
                , viewLibraryIsLocal (not (isAuthenticated m))
                , h2 [ class "subtitle is-6" ] [ text (String.fromInt p.length ++ " songs") ]
                ]
            ]
        , viewAnalysisBody
        ]

viewAnalysisTable : Playlist -> Html Msg
viewAnalysisTable p =
    div []
        [ table [ class "table is-fullwidth is-hoverable" ]
            [ thead []
                [ tr []
                    [ th [ style "padding-right" "5px"] [ ]
                    , th [ style "padding-left" "5px" ] [ text "BPM" ]
                    , th [] [ text "Key" ]
                    , th [] [ text "Energy" ]
                    , th [] [ text "Danceability" ]
                    ]
                ]
            , tbody [] (List.map viewAnalysisTableElement p.songs)
            ]
        ]

viewAnalysisTableElement : Song -> Html Msg
viewAnalysisTableElement s =
    tr []
        [ td [] 
            [ div [ class "media" ]
                [ div [ class "media-left" ]
                    [ figure [ class "image is-48x48" ]
                        [ img [ src s.imageUrl ] [] ]
                    ]
                , div [ class "media-content" ]
                    [ p [ style "font-weight" "bold" ] [ text (stringEllipsis s.name 40)]
                    , p [ class "is-size-7" ] [ text (stringEllipsis (String.join ", " s.artists) 50) ]
                    ]
                ]
            ]
        , td [] [ text (featureTempoToString s.tempo) ]
        , td [] [ text (featureKeyToString s.key)]
        , td [] [ text (featureConfidenceToString s.energy)]
        , td [] [ text (featureConfidenceToString s.danceability)]
        ]

viewAnalysisChart : Model -> Html Msg
viewAnalysisChart m =
    div [ class "box is-three-quarters-desktop" ]
        [ h1 [ class "title is-6" ] [ text "Playlist Flow" ]
        , h2 [ class "subtitle is-6" ] [ text "The following chart shows the tempo, energy and danceability of the songs in the playlist." ]
        , viewAnalysisChartSvg m
        ]
viewAnalysisChartSvg : Model -> Svg.Svg Msg
viewAnalysisChartSvg m =
    let
        data =
            List.indexedMap (\index song -> { index= (toFloat index), name = song.name, tempo=(song.tempo + 100), energy=(song.energy * 100), danceability=(song.danceability * 100 + 75)}) m.currentPlaylist.songs
            --List.indexedMap (\index song -> {index=index, song=song}) currentPlaylist.songs
    in
    C.chart
        [ CA.height 300
        , CA.width 1000
        , CE.onMouseMove OnChartHover (CE.getNearest CI.stacks)
        , CE.onMouseLeave (OnChartHover [])
        ]
        [ C.series .index
                [ C.interpolated .tempo
                    [ CA.monotone ]
                    []
                    --[ CA.circle, CA.color "white", CA.borderWidth 1 ]
                    |> C.named "Tempo"
                , C.interpolated .danceability
                    [ CA.monotone ]
                    []
                    --[ CA.circle, C ]
                    |> C.named "Danceability"
                , C.interpolated .energy
                    [ CA.monotone ]
                    []
                    --[ CA.circle, CA.color "white", CA.borderWidth 1 ]
                    |> C.named "Energy"
                ]
            data
        , C.legendsAt .min .max
            [ CA.column
            , CA.moveRight 15
            , CA.spacing 5
            ]
            [ CA.width 20 ]

        --, C.each m.chartHovering <| \p item ->
        --    [ C.tooltip item [] [] [] ]
        ]

viewLibrary : Model -> Html Msg
viewLibrary model =
    case model.playlists of
        [] ->
            div [ class "box is-three-quarters-desktop" ]
                [ h1 [ class "title is-1" ] [ text "Welcome to DJ Discover (beta)" ]
                , h2 [ class "subtitle is-6" ] [ text "With DJ Discover you can quickly access key metrics for your DJ set, such as BPM, Key and Dancability." ]
                , h2 [ class "subtitle is-6" ] [ text "To get started, authenticate with your Spotify account (top right) and select a playlist." ]
                , div [ class "mb-6" ] []
                , p [class "is-warning"] [ text "Due to Spotifys API restrictions I have to manually whitelist your account for access to this app. Please contact me at app@patjen.de to get whitelisted or use the demo data provided in the GitHub repo." ]
                , div [class "mb-6" ] []
                , p [] [ text "DJ Discover is a project by Patrick Jende, built with Elm and Spotify's Web API." ]
                ]
        _ ->
            div [ class "container is-three-quarters-desktop" ]
                [ div [ class "container is-flex is-justify-content-space-between", style "align-items" "center" ]
                    [ h1 [ class "title" ] [ text "Your Playlist Library" ]
                    , viewLibraryIsLocal (not (isAuthenticated model))
                    ]
                , div [ class "grid is-col-min-9 "]
                    (List.map viewLibraryPlaylist model.playlists)
                ]
    

viewLibraryPlaylist : Playlist -> Html Msg
viewLibraryPlaylist p =
    div [ class "cell"]
     [ a [ href ("/#" ++ p.id)]
            [ div [ class "card"]
                [ div [ class "card-image" ]
                    [ figure [ class "image is-1by1" ]
                        [ img [ src p.imageUrl ] []
                        ]
                    ]
                , div [ class "card-content" ]
                    [ div [ class "media" ]
                        [ div [ class "media-content" ]
                            -- TODO: Richtiges Ellipsenbilden basierend auf card größe
                            [ div [ class "title is-6 is-ellipsis" ] [ text (stringEllipsis p.name 22)]
                            , div [ class "subtitle is-size-7" ] [ text (String.fromInt p.length ++ " songs") ]
                            ]
                        ]
                    ]
                ]
            ]
        ]

viewLibraryIsLocal : Bool -> Html Msg
viewLibraryIsLocal isLocal =
    if isLocal then
        div [ class "icon-text has-text-info" ]
                [ span [ class "icon" ]
                    [ i [ class "fa-regular fa-floppy-disk" ] [] ]
                , text "Local Data only"
                ]
    else
        div [ class "icon-text has-text-success" ]
                [ span [ class "icon" ]
                    [ i [ class "fa-brands fa-spotify" ] [] ]
                , text "Spotify + Local Data"
                ]


viewDebug: Model -> Html Msg
viewDebug model =
    div []
      [
        text "The current URL is: "
        , b [] [ text (Url.toString model.url) ]
        , br [] []
        , text "The current playlist is: "
        , b [] [ text model.currentPlaylist.name ]
        , ul []
            [ viewDebugLink "#6SyUUxI1nSOW0fPE0HTp6F"
            , viewDebugLink "#6RKVFcQ9l8tYNJV4WgzfTi"
            , viewDebugLink "/login"
            , viewDebugLink ("#" ++ model.currentPlaylist.id)
            , button [ onClick ClickedResetCurrentPlaylist ] [ text "Reset current playlist" ]
            , button [ onClick ClickedReloadLibrary ] [ text "Reload library" ]
            , button [ onClick ClickedSortByDancability ] [ text "Sort by Danceability" ]
            , button [ onClick ClickedSortByEnergy ] [ text "Sort by Energy" ]
            ]
        ]
    

viewDebugLink : String -> Html msg
viewDebugLink path =
  li [] [ a [ href path ] [ text path ] ]


-- MARK: SVG


-- ENCODERS/DECODERS


songsDecoder : Decode.Decoder ( List Song, Maybe String )
songsDecoder =
    let
        songs =
            Decode.at [ "items" ]
                (Decode.list
                    (Decode.at [ "track" ] <|
                        Decode.map8
                            Song
                            (Decode.field "name" Decode.string)
                            (Decode.field "id" Decode.string)
                            (Decode.at [ "artists" ] (Decode.list (Decode.field "name" Decode.string)))
                            (Decode.at [ "album", "images" ] (Decode.index 0 (Decode.field "url" Decode.string)))
                            (Decode.succeed (0.0))
                            (Decode.succeed (0.0))
                            (Decode.succeed (0))
                            (Decode.succeed (0.0))
                            --(Decode.succeed (getTrackFeatures (Decode.field "id" Decode.string) ))
                    )
                )

        nextUrl =
            Decode.field "next" (Decode.nullable Decode.string)
    in
    Decode.map2 (\x y -> ( x, y )) songs nextUrl


playlistsDecoder : Decode.Decoder ( List Playlist, Maybe String )
playlistsDecoder =
    let
        playlists =
            Decode.at [ "items" ]
                (Decode.list <|
                    Decode.map5
                        (Playlist [])
                        (Decode.field "name" Decode.string)
                        (Decode.field "id" Decode.string)
                        (Decode.at [ "tracks" ] (Decode.field "total" Decode.int))
                        (Decode.field "snapshot_id" Decode.string)
                        (Decode.field "images" (Decode.index 0 (Decode.field "url" Decode.string)))
                )

        nextUrl =
            Decode.field "next" (Decode.nullable Decode.string)
    in
    Decode.map2 (\x y -> ( x, y )) playlists nextUrl

trackFeaturesDecoder : Decode.Decoder TrackFeatures
trackFeaturesDecoder =
    Decode.map5
        TrackFeatures
        (Decode.field "id" Decode.string)
        (Decode.field "danceability" Decode.float)
        (Decode.field "energy" Decode.float)
        (Decode.field "key" Decode.int)
        (Decode.field "tempo" Decode.float)

storageDecoder : Decode.Decoder (List Playlist)
storageDecoder =
    Decode.list <|
        Decode.map6
            Playlist
            (Decode.field "songs" <|
                Decode.list <|
                    Decode.map8
                        Song
                        (Decode.field "name" Decode.string)
                        (Decode.field "id" Decode.string)
                        (Decode.field "artists" (Decode.list Decode.string))
                        (Decode.field "imageUrl" Decode.string)
                        (Decode.field "danceability" Decode.float)
                        (Decode.field "energy" Decode.float)
                        (Decode.field "key" Decode.int)
                        (Decode.field "tempo" Decode.float)        
            )
            (Decode.field "name" Decode.string)
            (Decode.field "id" Decode.string)
            (Decode.field "length" Decode.int)
            (Decode.field "snapshot" Decode.string)
            (Decode.field "imageUrl" Decode.string)

        
storageEncoder : List Playlist -> Encode.Value
storageEncoder ps =
    let
        encodeSong s =
            Encode.object
                [ ( "name", Encode.string s.name )
                , ( "id", Encode.string s.id )
                , ( "artists", Encode.list Encode.string s.artists )
                , ( "imageUrl", Encode.string s.imageUrl )
                , ( "danceability", Encode.float s.danceability )
                , ( "energy", Encode.float s.energy )
                , ( "key", Encode.int s.key )
                , ( "tempo", Encode.float s.tempo )
                ]

        item p =
            Encode.object
                [ ( "songs", Encode.list encodeSong p.songs )
                , ( "name", Encode.string p.name )
                , ( "id", Encode.string p.id )
                , ( "length", Encode.int p.length )
                , ( "snapshot", Encode.string p.snapshot )
                , ( "imageUrl", Encode.string p.imageUrl )
                ]
    in
    Encode.list item ps



-- HELPERS

isAuthenticated : Model -> Bool
isAuthenticated model =
    model.authToken /= ""

featureKeyToString : Int -> String
featureKeyToString key =
    case key of
        0 ->
            "C"
        1 ->
            "C#"
        2 ->
            "D"
        3 ->
            "D#"
        4 ->
            "E"
        5 ->
            "F"
        6 ->
            "F#"
        7 ->
            "G"
        8 ->
            "G#"
        9 ->
            "A"
        10 ->
            "A#"
        11 ->
            "B"
        _ ->
            "Unknown"

featureTempoToString : Float -> String
featureTempoToString tempo =
    String.fromInt (round  tempo)

featureConfidenceToString : Float -> String
featureConfidenceToString confidence =
    String.fromInt (round (confidence*100)) ++ "%"

stringEllipsis : String -> Int -> String
stringEllipsis s n =
    if String.length s > n then
        String.left n s ++ "..."
    else
        s


defaultHttpsUrl : Url.Url
defaultHttpsUrl =
    { protocol = Https
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }


get : String -> Http.Expect Msg -> String -> Cmd Msg
get url expect tok =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" tok ]
        , url = url
        , body = Http.emptyBody
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


post : String -> String -> (Result Http.Error () -> Msg) -> Cmd Msg
post url tok f =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" tok ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectWhatever f
        , timeout = Nothing
        , tracker = Nothing
        }


getUrlFromPath : String -> String
getUrlFromPath path =
    { apiUrl | path = path } |> Url.toString
