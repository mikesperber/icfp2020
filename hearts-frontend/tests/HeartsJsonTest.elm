module HeartsJsonTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)

import Json.Encode
import Json.Decode

import HeartsJson exposing (..)
import HeartsGame exposing (..)

rankFuzzer : Fuzzer Rank
rankFuzzer =
    Fuzz.oneOf (List.map Fuzz.constant allRanks)

suitFuzzer : Fuzzer Suit
suitFuzzer =
    Fuzz.oneOf (List.map Fuzz.constant allSuits)

cardFuzzer : Fuzzer Card
cardFuzzer =
    Fuzz.map2 Card suitFuzzer rankFuzzer

jsonEncoding : Test
jsonEncoding =
    describe "decode . encode == id"
        [ fuzz cardFuzzer "holds for Card" <|
              \ card ->
                  card
                      |> encodeCard
                      |> Json.Decode.decodeValue cardDecoder
                      |> Expect.equal (Ok card)
        ]
