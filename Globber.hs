module Globber (matchGlob) where

type GlobPattern = String

matchGlob :: GlobPattern -> String -> Bool
matchGlob ('?':globRest) matcher  = case matcher of
    [] -> False
    (_:matcherRest) -> matchGlob globRest matcherRest
matchGlob xs ys = xs == ys
