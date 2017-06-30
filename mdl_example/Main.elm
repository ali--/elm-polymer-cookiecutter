module Main exposing (..)
import Html exposing (..)
import Http exposing (..)
import Html.Attributes exposing (href, class, style, attribute)
import Material
import Material.Card as Card
import Material.Color as Color
import Material.Textfield as Textfield
import Material.Scheme
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Icon as Icon
import Material.Tabs as Tabs
import Material.Table as Table
import Material.Progress as ProgressBar
import Material.Typography as Typography
import Material.Options as Options
import Material.Toggles as Toggles
import Json.Encode
import Json.Decode exposing (field)
import Set exposing (Set)
import Maybe

-- MODEL
type alias SparkURL = String
type alias FactTable = Maybe TableName

type alias Model =
    { errors : List String
    , column_descs : List ColumnDesc
    , table_descs : List TableDesc
    , dimension_columns : List ColumnDesc
    , fact_table : FactTable
    , spark_url : SparkURL
    , selected : Set String
    , tab : Int
     -- Boilerplate: model store for any and all Mdl components you use.
    , mdl : Material.Model 
    }
    
    
allSelected : Model -> Bool
allSelected model =
    Set.size model.selected == List.length model.column_descs

model : Model
model =
    { errors = []
    , column_descs = []
    , selected = Set.empty
    , spark_url = "local[*]"
    , table_descs = []
    , fact_table = Nothing
    , dimension_columns = []
    , tab = 0
    -- Boilerplate: Always use this initial Mdl model store.
    , mdl = Material.model
    }

-- ACTION, UPDATE

type Msg
    = SelectTab Int
    | SetSparkURL SparkURL
    | ConnectToSpark SparkURL
    | ConnectToSparkResult (Result Http.Error (List TableDesc))
    | RequestColumnDesc TableName ColumnName
    | RecieveColumnDesc (Result Http.Error ColumnDesc)
    | ChooseFactTable TableName
    -- dimension selection
    | Toggle String
    | ToggleAll
    -- Boilerplate: Msg clause for internal Mdl messages.
    | Mdl (Material.Msg Msg)


getAllColumnDescs : TableName -> List ColumnName -> Cmd Msg
getAllColumnDescs tableName columns =
    Cmd.batch (List.map (\col_name -> (getColumnDesc tableName col_name)) columns)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTab idx ->
          ({ model | tab = idx }, Cmd.none)

        RequestColumnDesc table_name column_name ->
          (model, getColumnDesc table_name column_name)

        RecieveColumnDesc (Ok newColumnDesc) ->
            let 
              non_new_descs = List.filter (\x -> not (x.name == newColumnDesc.name)) model.column_descs
            in
              ({model | column_descs =  non_new_descs ++ [newColumnDesc]}, Cmd.none)
            
        RecieveColumnDesc (Err err) ->
            ({model | errors = [(toString err)]}, Cmd.none)

        SetSparkURL string ->
            ({model | spark_url = string}, Cmd.none)
            
        ConnectToSpark url ->
            (model, getTableDesc url)
            
        ConnectToSparkResult (Ok newTableDescs) ->
            ({model | table_descs  = newTableDescs, tab = 1}, Cmd.none)
            
        ConnectToSparkResult (Err err) ->
            ({model | errors = [(toString err)]}, Cmd.none)
        
        ChooseFactTable newFactTable ->
            let
                factTableDesc = List.head (List.filter (\t -> t.name == newFactTable) model.table_descs)
                (factTableName, fetchColumnCmd) = case factTableDesc of
                    Just factTable ->
                        (Just newFactTable, (getAllColumnDescs factTable.name (List.map (\(a,b) -> a) factTable.columns)))
                    Nothing ->
                        (model.fact_table, Cmd.none)
            in
            ({model | fact_table = factTableName, tab = 2}, fetchColumnCmd)
        
        -- Boilerplate: Mdl action handler.
        Mdl msg_ -> Material.update Mdl msg_ model
        
        -- Choosing columns
        ToggleAll ->
           let 
              selected =
                if allSelected model then
                  Set.empty
                else
                  List.map key model.column_descs |> Set.fromList
              dimension_columns = model.column_descs
            in
              ({model | selected=selected,dimension_columns=dimension_columns}, Cmd.none)

        Toggle k ->
          let 
            selected =
                if Set.member k model.selected then
                  Set.remove k model.selected
                else
                  Set.insert k model.selected
            dimension_columns = List.filter (\c -> Set.member c.name selected) model.column_descs
          in
            ({model | selected=selected,dimension_columns=dimension_columns}, Cmd.none)

-- VIEW

sparkTab model = 
     div [] [
        Textfield.render Mdl [2] model.mdl
          [ Textfield.label "url of Spark Master"
          , Textfield.floatingLabel
          , Textfield.value model.spark_url
          , Options.onInput SetSparkURL
          ] []
        ,  
        Button.render Mdl 
            [2] 
            model.mdl
            [ Button.raised
            , Button.colored
            , Button.ripple
            , Options.onClick (ConnectToSpark model.spark_url)
            ]
            [ text "Connect"]
    ]

addDataSourceView : Model -> Html Msg
addDataSourceView model = div [] [
      Tabs.render Mdl [0] model.mdl
      [ Tabs.ripple
      , Tabs.onSelectTab SelectTab
      , Tabs.activeTab model.tab
      ]
      [ Tabs.label 
          [ Options.center ] 
          [ Icon.i "input"
          , Options.span [ css "width" "4px" ] []
          , text "Data" 
          ]
      , Tabs.label 
          [ Options.center, Options.disabled True ] 
          [ Icon.i "grid_on"
          , Options.span [ css "width" "4px" ] []
          , text "Facts" 
          ]
      , Tabs.label 
          [ Options.center ] 
          [ Icon.i "view_week"
          , Options.span [ css "width" "4px" ] []
          , text "Dimensions" 
          ]
      , Tabs.label 
          [ Options.center ] 
          [ Icon.i "launch"
          , Options.span [ css "width" "4px" ] []
          , text "Save" 
          ]
      ]
      [ Options.div 
          [ css "margin" "24px auto"
          , css "align-items" "flex-start"
          , Options.center
          , css "overflow-y" "auto"
          , css "height" "256px"
          ]
          [ case model.tab of
              0 -> sparkTab model
              1 -> tablesView model
              2 -> columnsView model
              3 -> ddlView model
              _ -> sparkTab model
          ]
      ]
    ]
-- view
white : Options.Property c m
white =
  Color.text Color.white

cardFooter model = [ 
         Options.span [ Typography.caption, Typography.contrast 0.87 ] [ text "Need help? Call us" ]
       , Button.render Mdl [8,1] model.mdl
          [ Button.icon, Button.ripple ]
          [ Icon.i "phone" ]
       , Button.render Mdl [8,2] model.mdl
          [ Button.icon, Button.ripple ]
          [ Icon.i "event_available" ]
      ]

cardHeader model = div [] [
        text "SNAP Cube Modeler (v 0.1)"     
    ]

cardView header child footer model = Card.view
  [ Color.background (Color.white)
  , css "width" "100%"
  , css "height" "100%"
  ]
  [ Card.title [ ] [ Card.head [ Color.text Color.black ] [ header model ] ]
    , Card.text [ Color.text Color.black ] [
        child model
       ,if not (model.errors == [])
            then (text ("Errors: "++(toString model.errors)))
            else (text "")
    ]
    , Card.actions
      [ Card.border, css "display" "flex"
      , css "justify-content" "space-between"
      , css "align-items" "center"
      , css "padding" "8px 16px 8px 16px"
      , Color.text Color.black ]
      (footer model)
  ]

view : Model -> Html Msg
view model = div [] [
    div
        [ style [ ( "padding", "2rem" ) ] ]
        [ cardView cardHeader addDataSourceView cardFooter model ]
        |> Material.Scheme.top
  ]

main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }
        
    
-- HTTP

apiURL = 
    "http://0.0.0.0:8888"

type alias ColumnName = String
type alias TableName = String
    
getColumnDesc : String -> String -> Cmd Msg
getColumnDesc table_name column_name =
  let
    url =
      apiURL ++ "/columns/" ++ table_name ++ "/" ++ column_name
  in
    Http.send RecieveColumnDesc (Http.get url decodeColumnDesc)

getTableDesc : String -> Cmd Msg
getTableDesc sparkUrl =
  let
    url =
      apiURL ++ "/tables"
  in
    Http.send ConnectToSparkResult (Http.get url (Json.Decode.list decodeTableDesc))

type alias TableDesc =
    { columns : List (String, String)
    , name : String
    }

decodeTableDesc : Json.Decode.Decoder TableDesc
decodeTableDesc =
    Json.Decode.map2 TableDesc
        (field "columns" decodeColumnTypes)
        (field "name" Json.Decode.string)
        
decodeColumnTypes : Json.Decode.Decoder (List (String, String))
decodeColumnTypes =
    Json.Decode.keyValuePairs Json.Decode.string
{-
encodeTableDesc : TableDesc -> Json.Encode.Value
encodeTableDesc record =
    Json.Encode.object
        [ ("columns",  Json.Encode.list <| List.map Json.Encode.list <| List.map Json.Encode.string <| record.columns)
        , ("name",  Json.Encode.string <| record.name)
        ]
-}  
-- Column Descriptions
type alias ColumnDesc =
    { name : String
    , count : Int
    , type_ : String
    , distinct_count : Int
    , dtype : String
    }

decodeColumnDesc : Json.Decode.Decoder ColumnDesc
decodeColumnDesc =
    Json.Decode.map5 ColumnDesc
        (field "name" Json.Decode.string)
        (field "count" Json.Decode.int)
        (field "type" Json.Decode.string)
        (field "distinct_count" Json.Decode.int)
        (field "dtype" Json.Decode.string)

encodeColumnDesc : ColumnDesc -> Json.Encode.Value
encodeColumnDesc record =
    Json.Encode.object
        [ ("name",  Json.Encode.string <| record.name)
        , ("count",  Json.Encode.int <| record.count)
        , ("type",  Json.Encode.string <| record.type_)
        , ("distinct_count",  Json.Encode.int <| record.distinct_count)
        , ("dtype",  Json.Encode.string <| record.dtype)
        ]
        
key : ColumnDesc -> String
key =
    .name
    

tablesView : Model -> Html Msg
tablesView model =
  if List.length model.table_descs > 0 then 
  Table.table [ ]
    [ Table.thead []
      [ Table.tr []
        [ Table.th [] [ text "Name" ]
        , Table.th [ Table.numeric ] [ text "Number of Columns" ]
        , Table.th [ ] [ text "Select as Fact Table" ]
        ]
      ]
    , Table.tbody []
        ( model.table_descs
          |> List.indexedMap (\idx item ->
               Table.tr
                 [  ]
                 [ Table.td [] [ text item.name ]
                 , Table.td [] [ text (toString (List.length item.columns)) ]
                 , Table.td [] [ chooseFactButton model item.name ]
                 ]
             )
        )
    ]
  else Html.text "Connect to Spark to choose a Fact Table"

chooseFactButton model factTableName = div [] [
 Button.render Mdl 
            [2] 
            model.mdl
            [ Button.raised
            , Button.colored
            , Button.ripple
            , Options.onClick ( ChooseFactTable factTableName )
            ]
            [ text "Choose as Fact Table"]
 ]


columnsView : Model -> Html Msg
columnsView model = 
  if List.length model.column_descs > 0 then div [] [
  Table.table [ Options.css "overflow" "auto" ]
    [ Table.thead []
      [ Table.tr []
        [ Table.th []
            [ Toggles.checkbox Mdl [-1] model.mdl
              [ Options.onToggle ToggleAll
              , Toggles.value (allSelected model)
              ] []
            ]
        , Table.th [] [ text "Name" ]
        , Table.th [] [ text "Data Type"]
        , Table.th [] [ text "Column Type"]
        , Table.th [ Table.numeric ] [ text "Count" ]
        , Table.th [ Table.numeric ] [ text "Distinct Count" ]
        ]
      ]
    , Table.tbody []
        ( model.column_descs
          |> List.indexedMap (\idx item ->
               Table.tr
                 [ Table.selected |> Options.when (Set.member (key item) model.selected) ]
                 [ Table.td []
                   [ Toggles.checkbox Mdl [idx] model.mdl
                     [ Options.onToggle (Toggle <| key item)
                     , Toggles.value <| Set.member (key item) model.selected
                     ] []
                   ]
                 , Table.td [] [ text item.name ]
                 , Table.td [] [ text item.dtype ]
                 , Table.td [] [ text item.type_ ]
                 , Table.td [ Table.numeric ] [ text (toString item.count) ]
                 , Table.td [ Table.numeric ] [ text (toString item.distinct_count) ]
                 ]
             )
        )
    ]
    ,
    selectedDimensionsView model
  ]
  else Html.text "Choose a Fact Table before selecting Dimensions"

selectedDimensionsView : Model -> Html Msg
selectedDimensionsView model =
  Table.table [ ]
    [ Table.thead []
      [ Table.tr []
        [ Table.th [] [ text "Name" ]
        , Table.th [ Table.numeric ] [ text "Number of Columns" ]
        , Table.th [ ] [ text "Notes" ]
        ]
      ]
    , Table.tbody []
        ( model.dimension_columns
          |> List.indexedMap (\idx item ->
               Table.tr
                 [  ]
                 [ Table.td [] [ text item.name ]
                 , Table.td [] [ text item.dtype ]
                 , Table.td [] [ text "None" ]
                 ]
             )
        )
    ]

ddlView model = 
    case model.fact_table of
        Just ft ->
            ddlTemplate { index_name = ft ++ "_index"
                         , source_table_name = ft
                         , dimensions = model.dimension_columns
                         , metrics = model.dimension_columns
                         , timestamp_dimensions = model.dimension_columns
                         , not_nullables = model.dimension_columns
                         , path = "/data/example-data"
                         , partition_by = model.dimension_columns
                         , index_timestamp = "l_shipdate"
                         } |> List.map (\t -> Html.text t) 
                           |> List.map (\t -> Html.p [] [t]) 
                           |> div []
        Nothing ->
            Html.text " Choose a Fact Table to see the generated SNAP Cube "
    
ddlTemplate v = [
  "create olap index  " ++ v.index_name ++ " on " ++ v.source_table_name,
  " dimensions \"" ++  (v.dimensions |> List.map (\d -> d.name) |> String.join ", " ) ++ "\"",
  " metrics \""    ++  (v.metrics    |> List.map (\m -> m.name) |> String.join ", " ) ++ "\""
  ] 
  ++
  (v.not_nullables |> List.map (\d -> " dimension " ++ d.name ++ " is not nullable"))
  ++
  (v.timestamp_dimensions 
       |> List.map (
           \d -> ( 
             " timestamp dimension " ++ d.name
             ++ (if d.name == v.index_timestamp then " is index timestamp " else "")
             ++ " spark timestamp format \"yyyy-MM-dd\" " 
             ++ " is nullable nullvalue \"1992-01-01T00:00:00.000\" "
           )
        )
  )
  ++
  [" Options ( " ++ "path \"" ++ v.path ++ "\"" ++ """ 
        rowFlushBoundary \"10000\",
        nonAggregateQueryHandling \"push_project_and_filters\",
        avgNumRowsPerPartition \"15000\",
        preferredSegmentSize \"50mb\",
        rowFlushBoundary \"1000\")
    """ 
  ]
 -- ++ [ "partition by " ++ (v.partition_by |> List.map (\p -> p.name) |> String.join ",") ++ ";" ]
 -- ++ [ "insert overwrite olap index " ++ v.index_name ++ " of " ++ v.source_table_name ++ ";" ]
-- ++ [ "insert overwrite olap index " ++ v.index_name ++ " of " ++ v.source_table_name ++ ";" ]
