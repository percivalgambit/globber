module Globber (matchGlob) where

type GlobPattern = String

matchGlob :: GlobPattern -> String -> Bool
matchGlob ('?':xs) (_:ys) = matchGlob xs ys
matchGlob xs ys = xs == ys
