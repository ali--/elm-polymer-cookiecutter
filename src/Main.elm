module Main exposing (..)

import Html exposing (Html, text, div, span, input, button, p, node, Attribute)
import Html.Attributes exposing (attribute,type_,defaultValue, class)
import Html.Events exposing (onInput, on, onClick, onCheck)
import Http
import Json.Decode as Decode exposing (float, at, string, Decoder, map2, map)
import Time exposing (Time, second)
import Geolocation
import Array
import Date
import Date.Extra
import Dict

import JsonPositionDecode exposing (Positions, Position, Devices, Device, decodeWS, decodeUser, User)
import WebSocket

import HttpBuilder exposing (..)
import RemoteData

-- routing / navigation
import UrlParser as Url
import Navigation

-- Polymer
import Polymer.Attributes exposing (icon, selected)
import Polymer.App as App
import Polymer.Paper as Paper
import Polymer.Events 


type Route =
    Home
    | Account
    | About
    | DeviceList
    | Map

parseRoute : Url.Parser (Route -> a) a
parseRoute =
    Url.oneOf
        [ Url.map Home Url.top
        , Url.map Account (Url.s "account")
        , Url.map About (Url.s "about")
        , Url.map DeviceList (Url.s "devices")
        , Url.map Map (Url.s "map")
        {-, Url.map Logout (s "logout")
        , Url.map Cards (s "cards")
        , Url.map Forms (s "forms")
        , Url.map NewUser (s "users" </> s "new")
        , Url.map DatePicker (s "date-picker")
        -}
        ]


routeView : Model -> Html Msg
routeView model =
    case currentRoute model of
        Just route ->
            case route of
                Home ->
                    div [] [
                        main_view model
                        ]

                Account ->
                    account_view model.user

                About ->
                    text "About Page" --View.Login.view model

                DeviceList ->
                    device_view model.devices

                Map ->
                    main_view model

                {-Routes.Logout ->
                    text "logging you out"

                NewUser ->
                    View.Signup.CreateUser.view model

                Cards ->
                    View.Cards.view model

                Forms ->
                    View.Forms.view model

                DatePicker ->
                    View.DatePicker.view model
                    -}
        Nothing ->
            text "404"

routeToString : Route -> String
routeToString route =
    case route of
        Home ->
            ""

        Account ->
            "account"

        About ->
            "about"

        DeviceList ->
            "devices"

        Map ->
            "map"

{-        Logout ->
            "logout"

        NewUser ->
            "users/new"

        Cards ->
            "cards"

        Forms ->
            "forms"

        DatePicker ->
            "date-picker"
        -}

currentRoute : Model -> Maybe Route
currentRoute model =
    List.head model.history
        |> Maybe.withDefault Nothing


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL

type alias Triple = (Date.Date, Float, Float)
type alias ReplayData = Array.Array Triple
type alias Model =
    { history : List (Maybe Route)
    , latitude : Float
    , longitude : Float
    , follow : Bool
    , positions : Positions
    , replay_data : RemoteData.WebData Positions
    , devices : RemoteData.WebData Devices
    , current_devices : Devices
    , selected_device : (Maybe Int)
    , current_t : Int
    , debug_info : String
    , user : User
    , play_speed : Int
    , dates : (Date.Date, Date.Date)
    , listen : Bool
    , username : String
    , password : String
    }

lat0 = 48.2082
long0 = 16.3738

init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( { history = [Url.parseHash parseRoute location]
        ,latitude = lat0
        , longitude = long0
        , follow = True
        , positions = [] 
        , replay_data = RemoteData.NotAsked
        , devices = RemoteData.NotAsked
        , current_devices = []
        , selected_device = Nothing
        , current_t = 0
        , debug_info = ""
        , user = Anonomous
        , play_speed = 0
        , dates = (Date.fromTime 0, Date.fromTime 0)
        , listen = False
        , username = ""
        , password = ""
    }
    , getSession Nothing)



-- UPDATE


type Msg
    = 
    UrlChange Navigation.Location
    | NewUrl Route
    | SetLatLong Float Float
    | CenterMap
    | Tick Time
    | NowMsg Geolocation.Location
    | ReplayDataRequested Int
    | ReplayDataRecieved (RemoteData.WebData JsonPositionDecode.Positions)
    | DevicesRequested
    | DevicesRecieved (RemoteData.WebData JsonPositionDecode.Devices)
    | DeviceSelected (Maybe Int)
    | SetTime String
    | SetSlider Float
    | PlayPressed Int
    | SetDate String Date.Date
    | WebSocketRecieved String
    | Login LoginMsg

type LoginMsg
    = SetUsername String
    | SetPassword String
    | AttemptLogin
    | LoginSuccess JsonPositionDecode.User
    | LoginFail String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        UrlChange location ->
            let
                newHistory =
                    Url.parseHash parseRoute location :: model.history
            in
                { model | history = newHistory } ! [] -- Ports.closeDrawer ()
        
        NewUrl route ->

            case route of
                {-Routes.Logout ->
                    { model
                        | apiKey = Nothing
                        , users = Model.initialUsersModel
                    }
                        |> update (NewUrl Routes.Login)
                -}
                DeviceList ->
                    model ! [Navigation.newUrl <| "#" ++ routeToString route
                            ,getDevices]

                _ ->
                    ( model
                    , Navigation.newUrl <| "#" ++ routeToString route
                    )

        SetLatLong latitude longitude ->
            { model | latitude = latitude, longitude = longitude } ! []

        SetTime time ->
            {model | debug_info = "Recieved Set Time " ++ time,current_t = time |> String.toInt |> Result.withDefault 0} ! []

        SetSlider when ->
            {model | debug_info = "Recieved Set Time " ++ (toString when), current_t = (Basics.round when)} ! []

        CenterMap -> 
            case model.positions |> List.reverse |> List.head of
                Just {latitude, longitude} ->
                    {model | latitude = latitude, longitude = longitude} ! []
                Nothing ->
                    (model, Cmd.none)
        Tick t->
            ({model | current_t = model.current_t + 1}, Cmd.none)

        NowMsg loc->
            model ! []

        ReplayDataRequested id ->
            ({model | debug_info = "Requested Replay Data for Device " ++ (toString id)
                    , replay_data = RemoteData.Loading}, 
                    (getReplayData id (Tuple.first model.dates) (Tuple.second model.dates)))

        ReplayDataRecieved response ->
            ({model | debug_info = "Recieved Replay Data" ++ toString response
                    , replay_data = response
                    , positions =
                        case response of
                          RemoteData.Success positions -> positions
                          _ -> model.positions
                    }, Cmd.none)

        DevicesRequested ->
            ({model | debug_info = "Requested List of Devices"
                    , devices = RemoteData.Loading
                    }, getDevices)

        DevicesRecieved response ->
            ({model | debug_info = "Recieved List of Devices" ++ (toString response)
                    , devices = response
                    , current_devices =
                        case response of
                          RemoteData.Success new_devices -> new_devices
                          _ -> model.current_devices
                    }, Cmd.none)

        DeviceSelected selection ->
            case selection of
                Nothing ->
                    model ! []
                Just id ->
                    {model | selected_device = (Just id)} ! [Navigation.newUrl "/"]

        PlayPressed speed ->
            {model | play_speed = speed} ! []

        SetDate when date ->
            let 
                _ = Debug.log ("Date Set " ++ when) (Date.Extra.toUtcIsoString date) 
            in 
                if when == "from"
                then {model | dates = (date, model.dates|>Tuple.second)} ! []
                else {model | dates = (model.dates|>Tuple.first, date)} ! []

        WebSocketRecieved str ->
            let 
                _ = Nothing -- Debug.log "Websocket: " <| decodeWS str
                {positions, devices} = decodeWS str
            in 
                {model | positions = model.positions ++ positions} ! []

        Login msg ->
            case msg of
                AttemptLogin ->
                    model ! [getSession <| Just (model.username, model.password)]
                SetUsername u ->
                    {model | username = u} ! []
                SetPassword p ->
                    {model | password = p} ! []
                LoginSuccess user->
                    {model | debug_info = model.debug_info ++ " Successful Login"
                        , listen = True
                        , username = user.email
                        } ! [Navigation.newUrl "/"]
                LoginFail why->
                    {model | debug_info = model.debug_info ++ " Login Failed" ++ why} ! []

host = "https://trackonroad.dyndns.org"
http_port = "443"

getReplayData : Int -> Date.Date -> Date.Date -> Cmd Msg
getReplayData deviceId from to =   
    let
        new_url = host ++ ":" ++ http_port ++ "/api/reports/route?_dc=1483500349501"
                    ++ "&deviceId=" ++ (toString deviceId)
                    ++ "&type=allEvents"
                    ++ "&from=" ++ (Date.Extra.toUtcIsoString from)
                    ++ "&to=" ++ (Date.Extra.toUtcIsoString to)
        _ = Debug.log "New URL: " new_url
        req = { method = "GET"
                , headers = [ Http.header "Authorization" "Basic YWRtaW46NjU0MTIzOTg3enh5"]
                , url = new_url
                , body = Http.emptyBody
                , expect = Http.expectJson JsonPositionDecode.decodePositions
                , timeout = Nothing
                , withCredentials = True
            }
    in
        Http.request req |> RemoteData.sendRequest |> Cmd.map ReplayDataRecieved

getDevices : Cmd Msg
getDevices =   
    HttpBuilder.get (host ++ ":" ++ http_port ++ "/api/devices")
      --  |> HttpBuilder.withQueryParams [("userId", "2")]
      --  |> HttpBuilder.withHeader "Authorization" "Basic YWRtaW46NjU0MTIzOTg3enh5"
        |> HttpBuilder.withExpect (Http.expectJson JsonPositionDecode.decodeDevices)   
        |> HttpBuilder.withCredentials
        |> HttpBuilder.toRequest
        |> RemoteData.sendRequest 
        |> Cmd.map DevicesRecieved

handleSessionRecieved : Result Http.Error JsonPositionDecode.User -> Msg
handleSessionRecieved result =
    case Debug.log "getSession Result" result of
        Ok user ->
            Login <| LoginSuccess user
        Err (Http.BadStatus e) ->
            if e.status.code == 404 then -- failed to reload session on startup
                NewUrl Account
            else
                Login <| LoginFail <| toString e
        Err e ->
            Login <| LoginFail <| toString e




getSession : Maybe (String, String) -> Cmd Msg
getSession creds =  
    case creds of
        Just (u, p)-> 
            let
                formdata = "email=" ++ u ++ "&password=" ++ p 
            in
            HttpBuilder.post (host ++ ":" ++ http_port ++ "/api/session")
                |> HttpBuilder.withExpect (Http.expectJson JsonPositionDecode.decodeUser)
                |> HttpBuilder.withStringBody "application/x-www-form-urlencoded; charset=UTF-8" formdata
                |> HttpBuilder.withCredentials
                |> HttpBuilder.send handleSessionRecieved

        Nothing ->
            HttpBuilder.get (host ++ ":" ++ http_port ++ "/api/session")
                |> HttpBuilder.withExpect (Http.expectJson JsonPositionDecode.decodeUser)
                |> HttpBuilder.withCredentials
                |> HttpBuilder.send handleSessionRecieved

googleMap : List (Attribute a) -> List (Html a) -> Html a
googleMap =
    Html.node "google-map"

mapMarker = 
    Html.node "google-map-marker"

view : Model -> Html Msg
view model =
    body model

body : Model -> Html Msg
body model = 
    App.headerLayout
        []
        [ App.header
            [ attribute "effects" "waterfall"
            ,  attribute "condenses" ""
            , attribute "reveals" ""
            ]
            [ App.toolbar
                []
                [ Paper.iconButton
                    [ attribute "icon" "menu"
                    ]
                    []
                , div
                    [ attribute "main-title" "" ]
                    [ text "TrackOnRoad" ]
                , div
                    []
                    [ text model.username ]
                , Paper.iconButton
                    [ icon "account-circle"
                    , Polymer.Events.onTap (NewUrl Account)
                    ]
                    [ ]
                ]
            ]
        , Paper.tabs [ selected "0", attribute "sticky" ""] 
            [
                Paper.tab [Polymer.Events.onTap (NewUrl Map)] 
                    [ text "Map"
                    ,Paper.iconButton [ icon "maps:map"] []
                ]
                ,Paper.tab [Polymer.Events.onTap (NewUrl DeviceList)] 
                    [ text "Devices"
                    ,Paper.iconButton [ icon "hardware:router"] []
                ]
            ]
        , div
            []
            [ routeView model ]
       , Paper.card
        [ attribute "heading" "Debug info", attribute "elevation" "2" ]
        [ div 
            [ class "card-content" ] 
            [ div [] [ text <| "Number of Data Points ="++(model.positions |> List.length |> toString) ]
            , div [] [ text <| "Debug: " ++ model.debug_info ] ]
        ]
        ]

replay_view : Int -> Model -> Html Msg
replay_view deviceId model = 
    div []
        (case model.replay_data of
            RemoteData.NotAsked ->
                [Paper.iconButton [class "big", icon "icons:cloud-download"
                , Polymer.Events.onTap <| ReplayDataRequested deviceId] []
                ]
            RemoteData.Loading ->
                [ text "Getting your data... ",  Paper.spinner [ attribute "active" "", class "big"] []]

            RemoteData.Failure error->
                [ text <| "Could not get replay data: " ++ (toString error) ++ " Please try again later"
                , Paper.iconButton [ icon "icons:cloud-download"
                   , Polymer.Events.onTap <| ReplayDataRequested deviceId] []
                ]   
            RemoteData.Success positions ->
                [Paper.slider
                    [ attribute "min" "0"
                    , attribute "max" <| toString <| Array.length <| positionsToTripplet model.positions
                    , attribute "value" <| toString model.current_t
                    , onSliderChange SetSlider
                    ]
                    []
                , div [] [
                   -- div [] [ text (toString (time, lat, long)) ],
                    Paper.iconButton 
                    (if model.play_speed == 0 then
                        [ icon "av:play-circle-filled", Polymer.Events.onTap <| PlayPressed 1, class "big"]
                    else if model.play_speed == 1 then
                        [ icon "av:forward-5", Polymer.Events.onTap <| PlayPressed 5, class "big"]
                    else if model.play_speed == 5 then
                        [ icon "av:forward-10", Polymer.Events.onTap <| PlayPressed 10, class "big"]
                    else if model.play_speed == 10 then
                        [ icon "av:forward-30", Polymer.Events.onTap <| PlayPressed 30, class "big"]
                    else
                        [ icon "av:pause-circle-filled", Polymer.Events.onTap <| PlayPressed 0, class "big"]
                    ) []
                    ,  Paper.button [ class "colored", onClick CenterMap ] [ text "Find Me!" ] 
                    ,  Paper.button [ class "colored", onClick (ReplayDataRequested deviceId)] [ text "Get Data!" ] 
                    ]
                ])

map_view : List (Device, Position) -> Html Msg
map_view deviceLocations =
    let
        _ = Debug.log "Displaying Devices: " deviceLocations
        createMarker : (Device, Position) -> Html Msg
        createMarker (d, p) = 
            mapMarker [ attribute "draggable" "false" 
                    , attribute "latitude" (toString p.latitude)
                    , attribute "longitude" (toString p.longitude) ] 
                    [ categoryIcon d.category
                    , text d.name 
                    ]

    in
    googleMap
        [ attribute "latitude" (toString lat0)
        , attribute "longitude" (toString long0)
        , attribute "api-key" "AIzaSyBy1e75iTCR5DW0xhj0_XEoxTgT1dDhC4g"
        , attribute "drag-events" "true"
        , attribute "fit-to-markers" "true"
        , recordLatLongOnDrag
        ]
        (List.map createMarker deviceLocations)


positionsToTripplet : Positions -> ReplayData
positionsToTripplet positions =
     positions 
     |> List.map (\pos -> (pos.fixTime,pos.latitude,pos.longitude))
     |> Array.fromList

recordLatLongOnDrag : Attribute Msg
recordLatLongOnDrag =
    on "google-map-drag" <|
        map2 SetLatLong
            (at [ "target", "latitude" ] float)
            (at [ "target", "longitude" ] float)

onSliderChange : (Float -> msg) -> Attribute msg
onSliderChange msg =
    on "value-changed" <|
        map msg
            (at [ "target", "value" ] float)

onChange : (Float -> Msg) -> Attribute Msg
onChange toMsg =
  Decode.string
    |> Decode.andThen decodeLatLong
    |> Decode.at [ "target", "value" ]
    |> Decode.map toMsg
    |> on "change"


decodeLatLong : String -> Decoder Float
decodeLatLong str =
    case Decode.decodeString Decode.float str of
        Ok num ->
            Decode.succeed (num / 10000)

        Err err ->
            Decode.fail err


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [Geolocation.changes NowMsg
        ,(if model.play_speed > 0
        then Time.every (second/(model.play_speed |> Basics.toFloat)) Tick
        else Sub.none)
        ,if model.listen == True
        then WebSocket.listen "wss://trackonroad.dyndns.org:443/api/socket" WebSocketRecieved
        else Sub.none]

devicePositions : Devices -> Positions -> List (Device, Positions)
devicePositions devices positions =
    let
        positionsOf id = List.filter (\p -> p.deviceId == id) positions
    in
        List.map (\d -> (d, positionsOf d.id)) devices

latestDevicePositions : List (Device, Positions) -> List (Device, Position)
latestDevicePositions devicePositions =
    let
        getLatestData : (Device, Positions) -> Maybe (Device, Position)
        getLatestData (device, positions) =
            case List.head positions of
                Just position ->
                    Just (device, position)
                Nothing ->
                    Nothing
    in
        List.map getLatestData devicePositions |> List.filterMap identity

main_view : Model -> Html Msg
main_view model =

    case model.selected_device of
        Nothing ->
            Paper.card
                [ attribute "elevation" "2" ]
                [ div [ class "card-content"] 
                    [devicePositions model.current_devices model.positions 
                        |> latestDevicePositions 
                        |> map_view
                    ] 
                ]

        Just deviceId ->
            let
                devices = model.current_devices |> List.filter (\d -> d.id == deviceId)
            in
                div
                [ class "view-date-picker" ]
                [ Paper.card
                    [ attribute "heading" "Choose what times you want to replay"
                    , attribute "elevation" "2"
                    ]
                    [ span [class "card-content"] 
                      [node "paper-datetime-picker-item" 
                        [ on "date-changed" (logDate "from")
                       -- , class "card-content"
                        , attribute "placeholder" "Starting On"
                        , icon "icons:today"
                        ]
                        []
                      ,node "paper-datetime-picker-item"
                        [ on "date-changed" (logDate "to")
                      --  , class "card-content"
                        , attribute "placeholder" "Ending At"
                        , icon "icons:date-range"
                        ]
                        []
                      ]
                    ]
                , Paper.card
                    [ attribute "elevation" "2" ]
                    [ div [ class "card-content"] [replay_view deviceId model] ]
                , Paper.card
                    [ attribute "elevation" "2" ]
                    [ div [ class "card-content"] 
                        [devicePositions devices model.positions 
                            |> latestDevicePositions 
                            |> map_view
                        ] 
                    ]
                ]

-- TODO Simply this using Json.Extra.Date
logDate : String -> Decode.Decoder Msg
logDate when =
    Decode.at [ "detail", "value" ] Decode.value
        |> Decode.map
            (\x ->
                (toString x) --|> Debug.log "toString"
                    |> String.dropLeft 1 --|> Debug.log "dropLeft"
                    |> String.dropRight 1 --|> Debug.log "dropRight"
                {-OLD-} --|> Date.Extra.fromIsoString |> Debug.log "fromIsoString"
                {-NEW-} |> Date.fromString --|> Debug.log "fromString"
                    |> Result.withDefault (Date.fromTime 10000)
                    |> (SetDate when)
            )

------------------------ DEVICES
device_view : RemoteData.WebData Devices -> Html Msg
device_view devices = 
    case devices of
        RemoteData.NotAsked ->
            text <| "Devices Not Requested Yet ..."

        RemoteData.Loading ->
            div [] 
                [ text "Getting your devices... ",  Paper.spinner [ attribute "active" "", class "big"] []]

        RemoteData.Failure error->
            div []    
                [ text <| "Could not get your devices: " ++ (toString error) ++ " Please try again later"
                , Paper.iconButton [ icon "icons:cloud-download"
                   , Polymer.Events.onTap <| DevicesRequested] []
                ]   
        RemoteData.Success devices ->
            Paper.card 
                [ attribute "heading" "Your Vehicles", attribute "elevation" "2" ]
                (List.map device_card devices)

categoryIcon category = 
        let
            category2html i = node "iron-icon" [ attribute "icon" i, attribute "item-icon" "" ] []
        in
            if category == "motorcycle" then
                    category2html "icons:motorcycle"
                else if category == "car" then
                    category2html "maps:directions-car"
                else if category == "bus" then
                    category2html "maps:directions-bus"
                else if category == "ship" then
                    category2html "maps:directions-boat"
                else if category == "truck" then
                    category2html "maps:local-shipping"
                else if category == "default" then
                    category2html "maps:my-location"
                else if category == "arrow" then
                    category2html "maps:near-me"
                else if category == "person" then
                    category2html "maps:directions-walk"
                else
                    category2html "icons:help"


device_card : JsonPositionDecode.Device -> Html Msg
device_card device =
    let
        status = if device.status == "online" then 
                    Paper.iconButton [ icon "device:signal-cellular-3-bar"] []
                 else if device.status == "offline" then 
                    Paper.iconButton [ icon "device:signal-cellular-off"] []
                 else if device.status == "" then
                    Paper.iconButton [ icon "device:signal-cellular-no-sim"] []
                 else if device.status == "unknown" then
                    Paper.iconButton [ icon "icons:help"] []
                 else 
                    text <| device.status
     
        lastUpdate = if device.lastUpdate == (Date.fromTime 0) then
                        text "This device has not yet sent any data"
                     else
                        text <| "Last Updated: " ++ (Date.Extra.toFormattedString 
                                                        "EEEE, MMMM d, y 'at' h:mm a" 
                                                        device.lastUpdate)

        model = if device.model /= "" then (" Model: " ++ device.model) else ""
        phone = if device.phone /= "" then (" Number: " ++ device.phone) else ""
        id = "Id: " ++ (toString device.id)

        replay = Paper.iconButton [ icon "av:play-circle-filled"
                                    , Polymer.Events.onTap <| DeviceSelected <| Just device.id
                                    , class "big"] []

    in
        div [ attribute "role" "listbox" ]
        [ Paper.iconItem []
            [ categoryIcon device.category
            , Paper.itemBody [attribute "two-line" ""]
                [div [] [text device.name]
                ,div [attribute "secondary" ""] [lastUpdate]
                ,div [attribute "secondary" ""] [text <| id ++ model ++ phone]
                ]
            , status
            , replay
            ]
        ]
------------------------ LOGIN

type User =
    Authenticated { name : String}
    | Anonomous
    | Admin

account_view : User -> Html Msg
account_view user =
    login_view user

login_view : User -> Html Msg
login_view model =
    div
        [ class "view-login" ]
        [ Paper.card
            [ attribute "heading" "Login"
            , attribute "elevation" "2"
            ]
            [ div
                [ class "card-content" ]
                [ Paper.input
                    [ Polymer.Attributes.label "Email"
                    , onInput <| Login << SetUsername
                    ]
                    []
                , Paper.input
                    [ Polymer.Attributes.label "Password"
                    , type_ "password"
                    , onInput <| Login << SetPassword
                    ]
                    []
                ]
            , div
                [ class "card-actions" ]
                [ Paper.button
                    [ class "colored"
                    , onClick <| Login AttemptLogin
                    ]
                    [ text "Login" ]
                ]
            ]
        ]