module Globber (matchGlob) where

import Data.List

type GlobPattern = String

matchGlob :: GlobPattern -> String -> Bool
matchGlob (globFirst:globRest) matched@(matchedFirst:matchedRest) = case globFirst of
    '\\' -> case globRest of
        ""  -> False
        (literalChar:rest)
            | literalChar == matchedFirst -> matchGlob rest matchedRest
            | otherwise                   -> False
    '*' -> case globRest of
        "" -> True
        _  -> or $ fmap (matchGlob globRest) (tails matched)
    '?' -> matchGlob globRest matchedRest
    _
        | globFirst == matchedFirst -> matchGlob globRest matchedRest
        | otherwise                 -> False
matchGlob ""  "" = True
matchGlob "*" "" = True
matchGlob _   "" = False
matchGlob ""  _  = False
