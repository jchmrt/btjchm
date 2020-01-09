module Beschrijver ( beschrijfSentence ) where

import Core

beschrijf :: Char -> String
beschrijf ' ' = "een spatie"
beschrijf '!' = "een uitroepteken"
beschrijf '"' = "een aanhalingsteken"
beschrijf '#' = "een hekje"
beschrijf '$' = "een dollarteken"
beschrijf '%' = "een procentteken"
beschrijf '&' = "een ampersand"
beschrijf '\'' = "een apostrof"
beschrijf '(' = "een haakje openen"
beschrijf ')' = "een haakje sluiten"
beschrijf '*' = "een asterisk"
beschrijf '+' = "een plus"
beschrijf ',' = "een komma"
beschrijf '-' = "een min-teken"
beschrijf '.' = "een punt"
beschrijf '/' = "een schuine streep naar voren"
beschrijf ':' = "een dubbele punt"
beschrijf ';' = "een puntkomma"
beschrijf '<' = "een kleiner-dan-teken"
beschrijf '=' = "een is-gelijkteken"
beschrijf '>' = "een groter-dan-teken"
beschrijf '?' = "een vraagteken"
beschrijf '@' = "een apenstaartje"
beschrijf '[' = "een blokhaak openen"
beschrijf '\\' = "een schuine streep naar achteren"
beschrijf ']' = "een blokhaak sluiten"
beschrijf '^' = "een dakje"
beschrijf '_' = "een laag streepje"
beschrijf '`' = "een accent grave"
beschrijf '{' = "een accolade openen"
beschrijf '|' = "een rechte streep"
beschrijf '}' = "een accolade sluiten"
beschrijf '~' = "een tilde"
beschrijf symbol
    | elem symbol (['a'..'z']++['A'..'Z']) = "de letter " ++ [symbol]
    | elem symbol (['0'..'9']) = "het getal " ++ [symbol]
    | otherwise = "het onbekende symbool '" ++ [symbol] ++ "'"



beschrijfSentence :: String -> String
beschrijfSentence [] = ""
beschrijfSentence string = sentence
    where
        symbol = head string
        amount = determineMultiples string
        amountStr = if amount > 0 then ((show amount) ++ " keer ") else ""
        remainder = drop amount string
        description = amountStr ++ (beschrijf symbol)
        sentence
            | amount == (length string) = "en " ++ description ++ "."
            | otherwise = description ++ ", " ++ (beschrijfSentence remainder)

determineMultiples :: String -> Int
determineMultiples (symbol:string) = countReoccurences symbol string 1

countReoccurences :: Char -> String -> Int -> Int
countReoccurences symbol "" count = count
countReoccurences symbol (next:rest) count
    | symbol == next = countReoccurences symbol rest (count + 1)
    | otherwise = count
