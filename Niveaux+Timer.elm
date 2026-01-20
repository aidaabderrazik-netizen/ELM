type Difficulte = Debutant | Intermediaire | Expert
type Mode = Classique | ContreLaMontre

type alias DonneesJeu =
    { mot : String
    , score : Int
    , tempsRestant : Int
    , difficulte : Difficulte
    , mode : Mode
    -- ...
    }

type Msg = Tick Time.Posix | ...

update msg model =
    case msg of
        Tick _ ->
            case model.status of
                EnJeu d ->
                    if d.mode == ContreLaMontre then
                        if d.tempsRestant <= 0 then
                            ( { model | status = Erreur "Temps écoulé !" }, Cmd.none )
                        else
                            ( { model | status = EnJeu { d | tempsRestant = d.tempsRestant - 1 } }, Cmd.none )
                    else ( model, Cmd.none )
                _ -> ( model, Cmd.none )
        
genererIndice : Difficulte -> String -> String
genererIndice diff mot =
    case diff of
        Debutant ->
            let 
                first = String.left 1 mot
                last = String.right 1 mot
                tirets = String.repeat (String.length mot - 2) " _ "
            in first ++ tirets ++ last
        
        Intermediaire -> 
            String.repeat (String.length mot) " _ "
        
        Expert -> 
            "Mot de " ++ String.fromInt (String.length mot) ++ " lettres"

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        EnJeu d -> 
            if d.mode == ContreLaMontre then 
                Time.every 1000 Tick 
            else 
                Sub.none
        _ -> 
            Sub.none