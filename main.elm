import Html exposing ( .. )
import Html.App
import Html.Attributes
import String
import List

-- TYPE ALIASES to help the model

type alias NodeId = Int
type alias NodeInfo = {name: String, id: NodeId, data: String}
type alias RangeInfo = NodeInfo
type alias SearchInfo = NodeInfo
type alias SetInfo = NodeInfo
type alias CombinatorInfo = NodeInfo
type Node = Range RangeInfo | Search SearchInfo | Set SetInfo | Product CombinatorInfo | Sum CombinatorInfo
type alias NodeList = List Node

type alias Link = { from:NodeId, to:NodeId }
type alias LinkList = List Link

type Page_State = Empty | Edit NodeId

type alias RangeData = (Int,Int,Int)

main = Html.App.beginnerProgram { model = default_model, view = view, update = update }

-- MODEL

type alias Model = { maxID:Int, state:Page_State, nodes:NodeList, links:LinkList }

default_model : Model
default_model = { maxID = 4, state = Empty,
  nodes = [ Product { name="", id=0, data="" },
            Set { name="Shaft Type", id=1, data="Keyed|D|Round" },
            Sum { name="", id=2, data="" },
            Range { name="Shaft Diameter", id=3, data="1.0|2.0|4" },
            Range { name="Shaft Diameter", id=4, data="3.0|4.0|4" }],
  links = [ {from=0,to=1}, {from=0,to=2}, {from=2,to=3}, {from=2,to=4} ] }

-- UPDATE

type Msg = Reset

update : Msg -> Model -> Model
update msg model =
  case msg of
    Reset -> default_model

-- VIEW

view : Model -> Html Msg
view model =
  case model.state of
    Empty ->
      div [] [ render_table model.nodes model.links 0 ]
    Edit nodeId ->
      div [] [ div [Html.Attributes.class "tree"] [ render_table model.nodes model.links 0 ],
        div [Html.Attributes.class "edit"] [ text " " ] ]

render_table : NodeList -> LinkList -> NodeId -> Html Msg
render_table nodeList linkList nodeId =
  let
    node = get_node nodeId nodeList
  in
    case node of
      Just node ->
        case node of
          Range info ->
            tr [] [ td [] [ text (info.name ++ ": " ++ (parse_range node)) ] ]
          Set info ->
            tr [] [ td [] [ text (info.name ++ ": " ++ (parse_set node)) ] ]
          Search info ->
            tr [] [ td [] [ text (info.name ++ ": " ++ "Not Implemented") ] ]
          Product info ->
            let
              child_ids = get_children nodeId linkList
            in
              table [] [ tbody [] [tr [] [ ( td [] [ (text "×") ] ), (td [] (List.map ( render_table nodeList linkList ) child_ids )) ]]]
          Sum info ->
            let
              child_ids = get_children nodeId linkList
            in
              table [] [ tbody [] [tr [] [ ( td [] [ (text "+") ] ), (td [] (List.map ( render_table nodeList linkList ) child_ids )) ]]]
      Nothing ->
        text "Error in fetching node"

get_info : Node -> NodeInfo
get_info node =
  case node of
    Range info ->
      info
    Set info ->
      info
    Search info ->
      info
    Product info ->
      info
    Sum info ->
      info

get_node : NodeId -> NodeList -> Maybe Node
get_node nodeId nodeList = List.head ( List.filter ( is_node nodeId ) nodeList )

is_node : NodeId -> Node -> Bool
is_node nodeId node = .id (get_info node) == nodeId

get_children : NodeId -> LinkList -> List NodeId
get_children nodeId linkList =
  linkList
    |> List.filter ( is_child nodeId )
    |> List.map .to

is_child : NodeId -> Link -> Bool
is_child nodeId tlink =
  if tlink.from == nodeId then True else False

clean_string : Maybe String -> String
clean_string s =
  case s of
    Just s ->
      s
    Nothing ->
      " "

get_index : Int -> List a -> Maybe a
get_index index list =
  List.head (List.drop index list)

parse_range_data : NodeInfo -> RangeData
parse_range_data info =
  let
    data_list = String.split "|" info.data
    npts = String.toFloat (clean_string (get_index 2 data_list))
    lowpt = String.toFloat (clean_string (get_index 0 data_list))
    highpt = String.toFloat (clean_string (get_index 1 data_list))
  in
    (npts, lowpt, highpt)

parse_range : Node -> String
parse_range node =
  let
    info = get_info node
    data_list = String.split "|" info.data
    npts = clean_string (get_index 2 data_list)
    lowpt = clean_string (get_index 0 data_list)
    highpt = clean_string (get_index 1 data_list)
    out_string = npts ++ " points between " ++ lowpt ++ " and " ++ highpt
  in
    case (String.length out_string) > 60 of
      True ->
        let
          out_string = String.slice 0 57 out_string
        in
          out_string ++ "..."
      False ->
        out_string

parse_set : Node -> String
parse_set node =
  let
    info = get_info node
    data_list = String.split "|" info.data
    out_string = String.join ", " data_list
  in
    case (String.length out_string) > 60 of
      True ->
        let
          out_string = String.slice 0 57 out_string
        in
          out_string ++ "..."
      False ->
        out_string

render_edit : NodeId -> NodeList -> Html Msg
render_edit nodeId nodeList =
  let
    node = get_node nodeId nodeList
  in
    case node of
      Just node ->
        case node of
          Set info ->
            text " "
          Range info ->
            text " "
          Search info ->
            text " "
          _ ->
            text " "
      Nothing ->
        text " "
