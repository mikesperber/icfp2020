module Shuffle exposing (shuffle, distribute)

import List.Extra exposing (getAt, removeAt)
import Random exposing (Seed)

shuffleHelper : Seed -> List a -> List a -> List a
shuffleHelper seed xs result =
    if List.isEmpty xs
    then result
    else
        let indexGenerator = Random.int 0 ((List.length xs) - 1)
            (index, nextSeed) = Random.step indexGenerator seed
            valAtIndex = getAt index xs
            xsWithoutValAtIndex = removeAt index xs
        in case valAtIndex of
               Just i ->
                   shuffleHelper nextSeed xsWithoutValAtIndex (i :: result)
               Nothing ->
                   Debug.todo "Index not in list"

shuffle : Seed -> List a -> List a
shuffle seed xs =
    shuffleHelper seed xs []

extract4 : List a -> List a
extract4 ys =
    case ys of
        [] -> []
        z :: zs -> z :: extract4 (List.drop 3 zs)

distribute : List a -> List (List a)
distribute xs =
    List.map (\i -> extract4 (List.drop i xs)) [0, 1, 2, 3]
