module JsonPositionDecode exposing (decodeWS
                                    ,decodePositions
                                    ,decodeDevices
                                    ,decodeUser
                                    ,User
                                    ,Position
                                    ,Positions
                                    ,Devices
                                    ,Device
                                    )
import Json.Encode
import Json.Decode
-- elm-package install -- yes noredink/elm-decode-pipeline
import Json.Decode.Pipeline
import Date exposing (Date)
import Json.Decode.Extra as DateDecoder

type alias Positions = List Position

decodePositions : Json.Decode.Decoder Positions
decodePositions = Json.Decode.list decodePosition

type alias Position =
    { id : Int
    , attributes : String
    , deviceId : Int
    , type_ : Maybe String
    , protocol : String
    , serverTime : Date
    , deviceTime : Date
    , fixTime : Date
    , outdated : Bool
    , valid : Bool
    , latitude : Float
    , longitude : Float
    , altitude : Float
    , speed : Float
    , course : Int
    , address : Maybe String
    }

type alias PositionAttributes =
    { index : Int
    , ignition : Bool
    , ac : Bool
    , adc1 : Int
    , adc2 : Int
    , odometer : Int
    , battery : Int
    , ip : String
    , distance : Float
    , totalDistance : Float
    }

decodePosition : Json.Decode.Decoder Position
decodePosition =
    Json.Decode.Pipeline.decode Position
        |> Json.Decode.Pipeline.required "id" (Json.Decode.int)
        |> Json.Decode.Pipeline.hardcoded "{}"
        |> Json.Decode.Pipeline.required "deviceId" (Json.Decode.int)
        |> Json.Decode.Pipeline.optional "type" (Json.Decode.maybe Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.required "protocol" (Json.Decode.string)
        |> Json.Decode.Pipeline.optional "serverTime" (DateDecoder.date) (Date.fromTime 0)
        |> Json.Decode.Pipeline.required "deviceTime" (DateDecoder.date)
        |> Json.Decode.Pipeline.required "fixTime" (DateDecoder.date)
        |> Json.Decode.Pipeline.required "outdated" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "valid" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "latitude" (Json.Decode.float)
        |> Json.Decode.Pipeline.required "longitude" (Json.Decode.float)
        |> Json.Decode.Pipeline.required "altitude" (Json.Decode.float)
        |> Json.Decode.Pipeline.required "speed" (Json.Decode.float)
        |> Json.Decode.Pipeline.required "course" (Json.Decode.int)
        |> Json.Decode.Pipeline.optional "address" (Json.Decode.maybe Json.Decode.string) Nothing

decodePositionAttributes : Json.Decode.Decoder PositionAttributes
decodePositionAttributes =
    Json.Decode.Pipeline.decode PositionAttributes
        |> Json.Decode.Pipeline.required "index" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "ignition" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "ac" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "adc1" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "adc2" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "odometer" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "battery" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "ip" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "distance" (Json.Decode.float)
        |> Json.Decode.Pipeline.required "totalDistance" (Json.Decode.float)

encodePosition : Position -> Json.Encode.Value
encodePosition record =
    Json.Encode.object
        [ ("id",  Json.Encode.int <| record.id)
        , ("attributes",  Json.Encode.string <| record.attributes)
        , ("deviceId",  Json.Encode.int <| record.deviceId)
        , ("type",  Json.Encode.string <| "the type")
        , ("protocol",  Json.Encode.string <| record.protocol)
        , ("serverTime",  Json.Encode.string <| toString <| record.serverTime)
        , ("deviceTime",  Json.Encode.string <| toString <| record.deviceTime)
        , ("fixTime",  Json.Encode.string <| toString <| record.fixTime)
        , ("outdated",  Json.Encode.bool <| record.outdated)
        , ("valid",  Json.Encode.bool <| record.valid)
        , ("latitude",  Json.Encode.float <| record.latitude)
        , ("longitude",  Json.Encode.float <| record.longitude)
        , ("altitude",  Json.Encode.float <| record.altitude)
        , ("speed",  Json.Encode.float <| record.speed)
        , ("course",  Json.Encode.int <| record.course)
        , ("address",  Json.Encode.string <| "the address")
        ]

encodePositionAttributes : PositionAttributes -> Json.Encode.Value
encodePositionAttributes record =
    Json.Encode.object
        [ ("index",  Json.Encode.int <| record.index)
        , ("ignition",  Json.Encode.bool <| record.ignition)
        , ("ac",  Json.Encode.bool <| record.ac)
        , ("adc1",  Json.Encode.int <| record.adc1)
        , ("adc2",  Json.Encode.int <| record.adc2)
        , ("odometer",  Json.Encode.int <| record.odometer)
        , ("battery",  Json.Encode.int <| record.battery)
        , ("ip",  Json.Encode.string <| record.ip)
        , ("distance",  Json.Encode.float <| record.distance)
        , ("totalDistance",  Json.Encode.float <| record.totalDistance)
        ]

type alias Event =
    { id : Int
    , attributes : String
    , deviceId : Int
    , type_ : String
    , serverTime : Date
    , positionId : Int
    , geofenceId : Int
    }


decodeEvent : Json.Decode.Decoder Event
decodeEvent =
    Json.Decode.Pipeline.decode Event
        |> Json.Decode.Pipeline.required "id" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "attributes" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "deviceId" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "type" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "serverTime" (DateDecoder.date)
        |> Json.Decode.Pipeline.required "positionId" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "geofenceId" (Json.Decode.int)

type alias Devices = List Device

decodeDevices : Json.Decode.Decoder Devices
decodeDevices = Json.Decode.list decodeDevice

type alias Device =
    { id : Int
    , attributes : String
    , name : String
    , uniqueId : String
    , status : String
    , lastUpdate : Date
    , positionId : Int
    , groupId : Int
    , geofenceIds : List Int
    , phone : String
    , model : String
    , contact : String
    , category : String
    }

decodeDevice : Json.Decode.Decoder Device
decodeDevice =
    Json.Decode.Pipeline.decode Device
        |> Json.Decode.Pipeline.required "id" (Json.Decode.int)
        |> Json.Decode.Pipeline.hardcoded "{}"
        |> Json.Decode.Pipeline.required "name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "uniqueId" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "status" (Json.Decode.string)
        |> Json.Decode.Pipeline.optional "lastUpdate" (DateDecoder.date) (Date.fromTime 0)
        |> Json.Decode.Pipeline.required "positionId" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "groupId" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "geofenceIds" (Json.Decode.list Json.Decode.int)
        |> Json.Decode.Pipeline.required "phone" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "model" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "contact" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "category" (Json.Decode.string)

type alias WebsocketMsg = 
    { positions : Positions
    , devices : Devices
    }

decodeWebsocketMsg : Json.Decode.Decoder WebsocketMsg
decodeWebsocketMsg =
    Json.Decode.Pipeline.decode WebsocketMsg
        |> Json.Decode.Pipeline.optional "positions" (decodePositions) []
        |> Json.Decode.Pipeline.optional "devices" (decodeDevices) []

decodeWS : String -> WebsocketMsg
decodeWS str =
    case Json.Decode.decodeString decodeWebsocketMsg str of
        Ok ws ->
            ws
        Err error  ->
            let 
                _ = Debug.log "decodeWS error" <| "error=" ++ error ++ " str=" ++ str
            in
                WebsocketMsg [] []

-- User

type alias User =
    { id : Int
    , attributes : String
    , name : String
    , email : String
    , readonly : Bool
    , admin : Bool
    , map : String
    , distanceUnit : String
    , speedUnit : String
    , latitude : Float
    , longitude : Float
    , zoom : Float
    , twelveHourFormat : Bool
    , coordinateFormat : String
    , disabled : Bool
    , expirationTime : String
    , deviceLimit : Int
    , token : String
    , password : String
    }

decodeUser : Json.Decode.Decoder User
decodeUser =
    Json.Decode.Pipeline.decode User
        |> Json.Decode.Pipeline.required "id" (Json.Decode.int)
        |> Json.Decode.Pipeline.hardcoded "{}"
        |> Json.Decode.Pipeline.required "name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "email" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "readonly" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "admin" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "map" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "distanceUnit" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "speedUnit" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "latitude" (Json.Decode.float)
        |> Json.Decode.Pipeline.required "longitude" (Json.Decode.float)
        |> Json.Decode.Pipeline.required "zoom" (Json.Decode.float)
        |> Json.Decode.Pipeline.required "twelveHourFormat" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "coordinateFormat" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "disabled" (Json.Decode.bool)
        |> Json.Decode.Pipeline.hardcoded "{}" --"expirationTime" (Json.Decode.maybe decodeComplexType)
        |> Json.Decode.Pipeline.required "deviceLimit" (Json.Decode.int)
        |> Json.Decode.Pipeline.hardcoded "{}"
        |> Json.Decode.Pipeline.hardcoded "{}"
