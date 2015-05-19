{-# LANGUAGE PatternGuards #-}

module Globber (matchGlob) where

import Data.List

type GlobPattern = String

matchGlob :: GlobPattern -> String -> Bool
matchGlob (globFirst:globRest) matched@(matchedFirst:matchedRest) = case globFirst of
    '\\'
        | (literalChar:rest) <- globRest
        , literalChar == matchedFirst -> matchGlob rest matchedRest
        | otherwise                   -> False
    '*' -> or $ fmap (matchGlob globRest) (tails matched)
    '?' -> matchGlob globRest matchedRest
    _
        | globFirst == matchedFirst -> matchGlob globRest matchedRest
        | otherwise                 -> False
matchGlob ""  "" = True
matchGlob "*" "" = True
matchGlob _   "" = False
matchGlob ""  _  = False
