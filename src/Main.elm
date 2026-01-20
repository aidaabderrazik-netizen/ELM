module Main exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Browser
import Html exposing (Html, div, text, pre)
import Http -- Sert à aller chercher le fichier
import Random
import Array exposing (Array)

type alias Meaning =
    { partOfSpeech : String
    , definitions : List Definition
    }


-- 1. Le Modèle : on stocke le contenu du fichier (du texte)
type Model
    = Chargement
    | Erreur String
    | Succes { tousLesMots : List String, motChoisi : String, meanings : List Meaning }

type alias Definition =
    String
-- 2. L'initialisation : on demande à Elm d'aller lire le fichier au démarrage
init : () -> ( Model, Cmd Msg )
init _ =
    ( Chargement
    , Http.get 
        { url = "mots.txt" -- Le nom de ton fichier
        , expect = Http.expectString ReçuLeFichier -- "Quand tu as fini de télécharger le texte, envoie-moi le message ReçuLeFichier".
        }
    )

-- 3. Les Messages : que peut-il se passer ?
type Msg
    = ReçuLeFichier (Result Http.Error String)
    |   IndexChoisi Int
    |   DefinitionRecue (Result Http.Error (List String))



definitionDecoder : Decoder String
definitionDecoder =
    Decode.field "definition" Decode.string

definitionsDecoder : Decoder (List String)
definitionsDecoder =
    Decode.field "definitions" (Decode.list definitionDecoder)

meaningsDecoder : Decoder Meaning
meaningsDecoder =
    Decode.map2 Meaning
        (Decode.list "partOfSpeech" Decode.string)
        definitionsDecoder 
        

dictionaryDecoder : Decoder (List Meaning)
dictionaryDecoder =
    Decode.list 
        (Decode.fielf "meanings" (Decode.list meaningDecoder))
        |> Decode.map List.concat




-- 4. L'Update : on traite la réponse du fichier
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReçuLeFichier resultat ->
            case resultat of
                Ok contenu ->
                    let 
                        ---- On sépare le texte en liste de mots (un mot par ligne)
                        listeMots = String.words contenu
                        taille=List.length listeMots - 1
                    in
                        ---On demande à ELM à generer un index au hasard
                     (Succes { tousLesMots = listeMots, motChoisi = "", definition=[""] }
                     , Random.generate IndexChoisi (Random.int 0 (taille))
                     )
                Err _ ->
                    ( Erreur "Impossible de lire le fichier .txt", Cmd.none )
        IndexChoisi index ->
            case model of
                
                Succes contenu->
                    let 
                        mot=List.drop index contenu.tousLesMots |> List.head |> Maybe.withDefault ""
                        url= "https://api.dictionaryapi.dev/api/v2/entries/en/" ++ mot
                    in 
                        (Succes { contenu | motChoisi=mot, definition =[""]}, 
                        Http.get {url=url,expect = Http.expectJson DefinitionRecue dictionaryDecoder})
                _-> 
                    ( Erreur "Liste non chargée", Cmd.none )
        DefinitionRecue resultat ->
            case (resultat, model) of
                ( Ok definitions, Succes data ) ->
                    ( Succes { data | definition = definitions }, Cmd.none )

                ( Err err,_)  ->
                    ( Erreur ("Erreur dictionnaire : " ++ Debug.toString err), Cmd.none )
                ( Ok _, _ ) -> (model ,Cmd.none)
                

-- 5. La Vue : on affiche le résultat
view : Model -> Html Msg
view model =
    case model of
        Chargement ->
            text "Lecture du fichier en cours..."

        Erreur message ->
            text message

        Succes contenu ->
             Html.div []
                (Html.text ("TROUVES LE MOT ")
                    :: List.map
                        (\def -> Html.p [] [ Html.text def ])
                        contenu.definition
                )


-- 6. Le Main
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }