module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, text, ul, li)

-- 1. TON MODÈLE : Une simple liste de mots
type alias Model =
    { mots : List String }

-- 2. TES DONNÉES : Copie tes mots ici (j'en mets quelques-uns pour l'exemple)
init : Model
init =
    { mots = 
        [ "apple"
        , "banana"
        , "computer"
        , "science"
        , "rocket"
        -- Tu peux copier tes 1000 mots ici en les mettant entre guillemets
        ] 
    }

-- 3. LA LOGIQUE : On ne change rien pour l'instant
update : msg -> Model -> Model
update _ model =
    model

-- 4. L'AFFICHAGE : On transforme la liste en HTML
view : Model -> Html msg
view model =
    div []
        [ h1 [] [ text "Mes Mots" ]
        , ul [] (List.map (\unMot -> li [] [ text unMot ]) model.mots)
        ]

-- 5. LE MAIN : La version la plus simple (sandbox)
main =
    Browser.sandbox 
        { init = init
        , update = update
        , view = view 
        }