module Globber (matchGlob) where

type GlobPattern = String

matchGlob :: GlobPattern -> String -> Bool
matchGlob ('*':globRest) matcher = case globRest of
    [] -> True
    _ -> matchGlob globRest (takeLast globRestLen matcher) where
        takeLast n = reverse . take n . reverse
        globRestLen = length globRest
matchGlob ('?':globRest) matcher  = case matcher of
    [] -> False
    (_:matcherRest) -> matchGlob globRest matcherRest
matchGlob (globFirst:globRest) (matcherFirst:matcherRest)
    | globFirst == matcherFirst = matchGlob globRest matcherRest
    | otherwise = False
matchGlob [] [] = True
matchGlob _ [] = False
matchGlob [] _ = False
