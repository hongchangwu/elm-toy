import Html exposing (Html, div, option, select, span, text)
import Html.Attributes exposing (selected)
import Html.Events exposing (on, targetValue)
import Json.Decode as Json

main =
  Html.beginnerProgram
    { model = defaultModel
    , update = update
    , view = view
    }

-- MODEL

type alias Model =
  { department : Department
  }

type Department
  = Books BooksModel
  | Electronics ElectronicsModel

type alias BooksModel =
  { genre : Genre
  }

type Genre
  = Business
  | Fiction
  | History
  | Science

type alias ElectronicsModel =
  { category : Category
  }

type Category
  = Computers
  | Phones
  | Printers
  | Televisions

defaultModel : Model
defaultModel = Model (Books defaultBooksModel)

defaultBooksModel : BooksModel
defaultBooksModel = BooksModel Fiction

defaultElectronicsModel : ElectronicsModel
defaultElectronicsModel = ElectronicsModel Computers

-- UPDATE

type Msg
  = UpdateDepartment Department
  | UpdateGenre Genre
  | UpdateCategory Category

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateDepartment newDepartment ->
      { model | department = newDepartment }

    UpdateGenre newGenre ->
      case model.department of
        Books books ->
          { model | department = Books { books | genre = newGenre } }

        _ ->
          model

    UpdateCategory newCategory ->
      case model.department of
        Electronics electronics ->
          { model | department = Electronics { electronics | category = newCategory } }

        _ ->
          model

-- VIEW

departmentView : Department -> Html Msg
departmentView department =
  let
    updateDepartment s =
      case s of
        "Books" ->
          UpdateDepartment (Books defaultBooksModel)

        "Electronics" ->
          UpdateDepartment (Electronics defaultElectronicsModel)

        _ ->
          UpdateDepartment department
  in
  div
    []
    [ span
        []
        [ text "Department: " ]
    , select
        [ on "change"
            (Json.map updateDepartment targetValue)
        ]
        [ option
            [ selected
                (case department of
                   Books _ -> True
                   _ -> False)
            ]
            [ text "Books" ]
        , option
            [ selected
                (case department of
                   Electronics _ -> True
                   _ -> False)
            ]
            [ text "Electronics" ]
        ]
    ]

booksView : BooksModel -> Html Msg
booksView books =
  let
    updateGenre s =
      case s of
        "Business" ->
          UpdateGenre Business

        "Fiction" ->
          UpdateGenre Fiction

        "History" ->
          UpdateGenre History

        "Science" ->
          UpdateGenre Science

        _ ->
          UpdateGenre books.genre
  in
  div
    []
    [ span
        []
        [ text "Genre: " ]
    , select
        [ on "change"
            (Json.map updateGenre targetValue)
        ]
        (List.map
           (\genre ->
              option
                [ selected (genre == books.genre) ]
                [ text (toString genre) ])
           [ Business, Fiction, History, Science ])
    ]

electronicsView : ElectronicsModel -> Html Msg
electronicsView electronics =
  let
    updateCategory s =
      case s of
        "Computers" ->
          UpdateCategory Computers

        "Phones" ->
          UpdateCategory Phones

        "Printers" ->
          UpdateCategory Printers

        "Televisions" ->
          UpdateCategory Televisions

        _ ->
          UpdateCategory electronics.category
  in
  div
    []
    [ span
        []
        [ text "Category: " ]
    , select
        [ on "change"
            (Json.map updateCategory targetValue)
        ]
      (List.map
         (\category ->
            option
              [ selected (category == electronics.category) ]
              [ text (toString category) ])
         [ Computers, Phones, Printers, Televisions ])
    ]


view : Model -> Html Msg
view model =
  div
    []
    [ departmentView model.department
    , case model.department of
        Books books ->
          booksView books

        Electronics electronics ->
          electronicsView electronics
    ]
