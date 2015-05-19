module Globber (matchGlob) where

type GlobPattern = String

matchGlob :: GlobPattern -> String -> Bool
matchGlob glob@(globFirst:globRest) matched = case matched of
    "" | glob == "*" -> True
       | otherwise   -> False
    (matchedFirst:matchedRest) -> case globFirst of
        '\\'
            | globRest == "" -> False
            | head globRest == matchedFirst -> matchGlob (tail globRest) matchedRest
            | otherwise -> False
        '*'
            | globRest == "" -> True
            | otherwise -> matchGlob globRest (takeLast globRestLen matched) where
                takeLast n = reverse . take n . reverse
                globRestLen = length globRest
        '?' -> matchGlob globRest matchedRest
        _
            | globFirst == matchedFirst -> matchGlob globRest matchedRest
            | otherwise -> False
matchGlob "" "" = True
matchGlob "" _ = False
