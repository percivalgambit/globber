module Globber (matchGlob) where

import Data.List

type GlobPattern = String

matchGlob :: GlobPattern -> String -> Bool
matchGlob glob@(globFirst:globRest) matched = case matched of
    "" | glob == "*" -> True
       | otherwise   -> False
    (matchedFirst:matchedRest) -> case globFirst of
        '\\'
            | globRest == ""                -> False
            | head globRest == matchedFirst -> matchGlob (tail globRest) matchedRest
            | otherwise                     -> False
        '*'
            | globRest == "" -> True
            | otherwise      -> or $ fmap (matchGlob globRest) (tails matched)
        '?' -> matchGlob globRest matchedRest
        _
            | globFirst == matchedFirst -> matchGlob globRest matchedRest
            | otherwise                 -> False
matchGlob "" "" = True
matchGlob "" _  = False
