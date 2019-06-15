{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "effect"
    , "console"
    , "debug"
    , "psci-support"
    , "tuples"
    , "strings"
    , "ordered-collections"
    , "profunctor-lenses"
    , "spec"
    ]
, packages =
    ./packages.dhall
}
