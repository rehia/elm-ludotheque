module Main exposing (..)

import Browser
import Html exposing (Html, button, div, form, h5, input, label, small, text)
import Html.Attributes exposing (class, for, href, id, placeholder, src, type_, value)
import Html.Attributes.Aria exposing (ariaDescribedby)
import Html.Events exposing (onInput)
import Regex


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


init : Model
init =
    { ludotheque = initLodotheque
    , recherche = initRecherche
    }


initLodotheque : Ludotheque
initLodotheque =
    [ unJeu "Jack le Pirate" "France Cartes" Nothing ( 2, 4 ) (Just 6) 15
    , unJeu "Mr Jack" "Hurrican" Nothing ( 2, 2 ) (Just 9) 30
    , unJeu "Splendor" "Space Cowboys" Nothing ( 2, 4 ) (Just 10) 30
    ]


initRecherche : Recherche
initRecherche =
    { nom = ""
    , ageMinimum = 0
    , nombreJoueurs = 0
    }


type alias Range =
    ( Int, Int )


type Jeu
    = Jeu
        { nom : String
        , editeur : String
        , photo : Maybe String
        , nombreJoueurs : Range
        , ageMinimum : Maybe Int
        , dureeConseillee : Int
        }


unJeu : String -> String -> Maybe String -> Range -> Maybe Int -> Int -> Jeu
unJeu nom editeur photo nbJoueurs ageMini duree =
    Jeu
        { nom = nom
        , editeur = editeur
        , photo = photo
        , nombreJoueurs = nbJoueurs
        , ageMinimum = ageMini
        , dureeConseillee = duree
        }


type alias Ludotheque =
    List Jeu


type alias Recherche =
    { nom : String
    , ageMinimum : Int
    , nombreJoueurs : Int
    }


type alias Model =
    { ludotheque : Ludotheque
    , recherche : Recherche
    }


type Msg
    = FiltrerSurNom String
    | FiltrerSurAgeMinimum Int
    | FiltrerSurNombreJoueurs Int


dePatternARegex : String -> Regex.Regex
dePatternARegex =
    Maybe.withDefault Regex.never << Regex.fromStringWith { caseInsensitive = True, multiline = False }


matchPattern : String -> String -> Bool
matchPattern pattern =
    Regex.contains (dePatternARegex pattern)


chercheSurNom : String -> Jeu -> Bool
chercheSurNom pattern (Jeu { nom }) =
    matchPattern pattern nom


estJouablePar : Int -> Jeu -> Bool
estJouablePar nombre (Jeu { nombreJoueurs }) =
    dansLeRange nombre nombreJoueurs


dansLeRange : Int -> Range -> Bool
dansLeRange nombre ( mini, maxi ) =
    nombre >= mini && nombre <= maxi


peutJouerA : Int -> Jeu -> Bool
peutJouerA age (Jeu { ageMinimum }) =
    (>=) age << Maybe.withDefault 0 <| ageMinimum


updateLudotheque : Msg -> Ludotheque -> Ludotheque
updateLudotheque msg ludotheque =
    case msg of
        FiltrerSurNom pattern ->
            case pattern of
                "" ->
                    initLodotheque

                otherwise ->
                    List.filter (chercheSurNom pattern) ludotheque

        FiltrerSurAgeMinimum age ->
            List.filter (peutJouerA age) ludotheque

        FiltrerSurNombreJoueurs nombre ->
            List.filter (estJouablePar nombre) ludotheque


updateRecherche : Msg -> Recherche -> Recherche
updateRecherche msg recherche =
    case msg of
        FiltrerSurNom pattern ->
            { recherche | nom = pattern }

        FiltrerSurAgeMinimum age ->
            { recherche | ageMinimum = age }

        FiltrerSurNombreJoueurs nombre ->
            { recherche | nombreJoueurs = nombre }


update : Msg -> Model -> Model
update msg model =
    { model | ludotheque = updateLudotheque msg model.ludotheque }


view : Model -> Html Msg
view model =
    div [ class "container-fluid" ]
        [ viewRecherche model.recherche
        , div [ class "list-group" ]
            (List.map viewJeu model.ludotheque)
        ]


viewJeu : Jeu -> Html Msg
viewJeu (Jeu { nom, editeur }) =
    div [ class "list-group-item" ]
        [ h5 [] [ text nom ]
        , small [] [ text editeur ]
        ]


viewRecherche : Recherche -> Html Msg
viewRecherche model =
    form []
        [ div [ class "form-group" ]
            [ label [ for "nom" ]
                [ text "Nom du livre" ]
            , input
                [ type_ "text"
                , class "form-control"
                , id "nom"
                , ariaDescribedby "aideNom"
                , placeholder "Nom du livre"
                , onInput FiltrerSurNom
                ]
                []
            , small [ id "aideNom", class "form-text text-muted" ]
                [ text "Saisissez tout ou partie du nom du livre." ]
            ]
        ]
