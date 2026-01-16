module Main exposing (..)

import Browser
import Html exposing (Html, div, text, pre)
import Http -- Sert à aller chercher le fichier

-- 1. Le Modèle : on stocke le contenu du fichier (du texte)
type Model
    = Chargement
    | Erreur String
    | Succes String
 
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

-- 4. L'Update : on traite la réponse du fichier
update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        ReçuLeFichier résultat ->
            case résultat of
                Ok contenu ->
                    ( Succes contenu, Cmd.none )

                Err _ ->
                    ( Erreur "Impossible de lire le fichier .txt", Cmd.none )

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
            pre [] [ text contenu ]

-- 6. Le Main
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }