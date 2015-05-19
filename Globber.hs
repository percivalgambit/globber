module Globber (matchGlob) where

type GlobPattern = String

matchGlob :: GlobPattern -> String -> Bool
matchGlob ('\\':literalChar:globRest) (matchedFirst:matchedRest)
    | literalChar == matchedFirst = matchGlob globRest matchedRest
    | otherwise = False
matchGlob "\\" _ = False
matchGlob ('*':globRest) matched = case globRest of
    [] -> True
    _ -> matchGlob globRest (takeLast globRestLen matched) where
        takeLast n = reverse . take n . reverse
        globRestLen = length globRest
matchGlob ('?':globRest) matched  = case matched of
    [] -> False
    (_:matchedRest) -> matchGlob globRest matchedRest
matchGlob (globFirst:globRest) (matchedFirst:matchedRest)
    | globFirst == matchedFirst = matchGlob globRest matchedRest
    | otherwise = False
matchGlob [] [] = True
matchGlob _ [] = False
matchGlob [] _ = False
