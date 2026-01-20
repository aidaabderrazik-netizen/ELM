module Main exposing (..)

import Browser
import Html exposing (Html, div, text, pre)
import Http -- Sert à aller chercher le fichier
import Random
import Array exposing (Array)

-- 1. Le Modèle : on stocke le contenu du fichier (du texte)
type Model
    = Chargement
    | Erreur String
    | Succes { tousLesMots : List String, motChoisi : String }
 
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
                     (Succes { tousLesMots = listeMots, motChoisi = "" }
                     , Random.generate IndexChoisi (Random.int 0 (taille))
                     )
                Err _ ->
                    ( Erreur "Impossible de lire le fichier .txt", Cmd.none )
        IndexChoisi index ->
            case model of
                
                Succes contenu->
                    let 
                        mot=List.drop index contenu.tousLesMots |> List.head |> Maybe.withDefault ""
                    in 
                        (Succes { contenu | motChoisi=mot}, Cmd.none)
                _-> 
                    ( Erreur "Liste non chargée", Cmd.none )

-- 5. La Vue : on affiche le résultat
view : Model -> Html Msg
view model =
    case model of
        Chargement ->
            text "Lecture du fichier en cours..."

        Erreur message ->
            text message

        Succes contenu ->
            -- "pre" garde le formatage (un mot par ligne) du fichier texte
            text ("Mot aléatoire : "++ contenu.motChoisi)

-- 6. Le Main
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }