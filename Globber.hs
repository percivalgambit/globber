module Globber (matchGlob) where

import Data.List

type GlobPattern = String

matchGlob :: GlobPattern -> String -> Bool
matchGlob ('\\':literalChar:globRest) (matchedFirst:matchedRest)
    | literalChar == matchedFirst = matchGlob globRest matchedRest
    | otherwise = False
matchGlob "\\" _ = False
matchGlob ('*':globRest) matched = case globRest of
    [] -> True
    _  -> or $ fmap (matchGlob globRest) (tails matched)
matchGlob ('?':globRest) matched  = case matched of
    [] -> False
    (_:matchedRest) -> matchGlob globRest matchedRest
matchGlob (globFirst:globRest) (matchedFirst:matchedRest)
    | globFirst == matchedFirst = matchGlob globRest matchedRest
    | otherwise = False
matchGlob [] [] = True
matchGlob _ [] = False
matchGlob [] _ = False
