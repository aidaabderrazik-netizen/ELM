module Main exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Browser
import Html exposing (Html, div, text, header, h1, p, input)
import Html.Attributes exposing (style, placeholder, value, class)
import Http -- Sert à aller chercher le fichier
import Random
import Array exposing (Array)
import Dict exposing (Dict)
import Html.Events exposing (onInput)

import List exposing (sortBy)

type alias Meaning =
    { partOfSpeech : String
    , definitions : List Definition
    }


-- 1. Le Modèle : on stocke le contenu du fichier (du texte)
type Model
    = Chargement
    | Erreur String
    | Succes { tousLesMots : List String, motChoisi : String, meanings : List Meaning, saisie : String }

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
    |   DefinitionsRecues (Result Http.Error (List Meaning))
    |   ChangementSaisie String

partOfSpeechPriority : String -> Int
partOfSpeechPriority pos =
    case pos of
        "noun" -> 0
        "verb" -> 1
        "proper noun" -> 2
        _ -> 99
insertMeaning : Meaning -> Dict String Meaning -> Dict String Meaning
insertMeaning meaning dict =
    Dict.update meaning.partOfSpeech
        (\maybeExisting ->
            case maybeExisting of
                Nothing ->
                    Just meaning

                Just existing ->
                    Just
                        { existing
                            | definitions =
                                existing.definitions ++ meaning.definitions
                        }
        )
        dict
groupMeanings : List Meaning -> List Meaning
groupMeanings meanings =
    meanings
        |> List.foldl insertMeaning Dict.empty
        |> Dict.values
        |> List.sortBy (\m -> partOfSpeechPriority m.partOfSpeech)

definitionDecoder : Decoder String
definitionDecoder =
    Decode.field "definition" Decode.string

definitionsDecoder : Decoder (List String)
definitionsDecoder =
    Decode.field "definitions" (Decode.list definitionDecoder)

meaningsDecoder : Decoder Meaning
meaningsDecoder =
    Decode.map2 Meaning
        (Decode.field "partOfSpeech" Decode.string)
        definitionsDecoder 
        

dictionaryDecoder : Decoder (List Meaning)
dictionaryDecoder =
    Decode.list 
        (Decode.field "meanings" (Decode.list meaningsDecoder))
        |> Decode.map List.concat

-- AJOUT DES STYLES POUR LE CLIGNOTEMENT
stylesAnimation : Html Msg
stylesAnimation =
    Html.node "style" []
        [ text """
            @keyframes blink-green {
                0%, 100% { background-color: #6aaa64; color: white; border-color: #6aaa64; }
                50% { background-color: white; color: #121213; border-color: #d3d6da; }
            }
            .wordle-success {
                animation: blink-green 0.6s ease-in-out infinite;
                background-color: #6aaa64 !important;
                color: white !important;
                border-color: #6aaa64 !important;
            }
        """ ]


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
                     (Succes { tousLesMots = listeMots, motChoisi = "", meanings=[{partOfSpeech ="", definitions=[""]}], saisie = "" }
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
                        (Succes { contenu | motChoisi=mot, meanings =[{partOfSpeech ="", definitions=[""]}], saisie = ""}, 
                        Http.get {url=url,expect = Http.expectJson DefinitionsRecues dictionaryDecoder})
                _-> 
                    ( Erreur "Liste non chargée", Cmd.none )
        DefinitionsRecues resultat ->
            case (resultat, model) of
                ( Ok meanings, Succes data ) ->
                    ( Succes { data | meanings = meanings }, Cmd.none )

                ( Err err,_)  ->
                    ( Erreur ("Erreur dictionnaire : " ++ Debug.toString err), Cmd.none )
                ( Ok _, _ ) -> (model ,Cmd.none)

        ChangementSaisie nouvelleSaisie ->
            case model of
                Succes data ->
                    ( Succes { data | saisie = nouvelleSaisie }, Cmd.none )

                _ ->
                    ( model, Cmd.none )
                

-- 5. La Vue : on affiche le résultat
viewMeaning : Meaning -> Html Msg
viewMeaning meaning =
    div [ style "margin-bottom" "24px" ]
        [ div 
            [ style "color" "#818384"
            , style "text-transform" "uppercase"
            , style "font-weight" "bold"
            , style "font-size" "14px"
            , style "margin-bottom" "8px"
            ] 
            [ text meaning.partOfSpeech ]
        , div [ style "color" "#121213", style "line-height" "1.5" ] 
            (List.map (\def -> p [ style "margin" "4px 0" ] [ text ("• " ++ def) ]) meaning.definitions)
        ]

view : Model -> Html Msg
view model =
    case model of
        Chargement ->
            div [ style "background-color" "white", style "height" "100vh", style "color" "black", style "padding" "20px" ] [ text "Chargement du dictionnaire..." ]

        Erreur message ->
            div [ style "background-color" "white", style "height" "100vh", style "color" "black", style "padding" "20px" ] [ text message ]

        Succes data ->
            let
                estJuste = String.toLower (String.trim data.saisie) == String.toLower data.motChoisi
            in
            div 
                [ style "background-color" "white"
                , style "color" "#ffffff"
                , style "min-height" "100vh"
                , style "display" "flex"
                , style "flex-direction" "column"
                , style "align-items" "center"
                , style "font-family" "'Helvetica Neue', Arial, sans-serif"
                ]
                [ stylesAnimation
                , header 
                    [ style "width" "100%"
                    , style "background-color" "white"
                    , style "color" "#121213"
                    , style "border-bottom" "1px solid #d3d6da"
                    , style "text-align" "center"
                    , style "padding" "15px 0"
                    , style "margin-bottom" "30px"
                    ]
                    [ h1 [ style "margin" "0", style "font-size" "32px", style "letter-spacing" "1px" ] [ text "GUESS IT !" ] ]

                , div [ style "max-width" "1200px", style "width" "90%" ]
                    [ div [ style "margin-bottom" "30px" ] 
                        (List.map viewMeaning data.meanings)
                    
                    , input 
                        [ placeholder "Type your guess..."
                        , value data.saisie
                        , onInput ChangementSaisie 
                        , class (if estJuste then "wordle-success" else "")
                        , style "background-color" "white"
                        , style "border" "2px solid #d3d6da"
                        , style "color" "#121213"
                        , style "padding" "15px"
                        , style "width" "100%"
                        , style "font-size" "1.2rem"
                        , style "text-align" "center"
                        , style "text-transform" "uppercase"
                        , style "box-sizing" "border-box"
                        , style "outline" "none"
                        ]
                        []
                    ]
                ]
-- 6. Le Main
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }