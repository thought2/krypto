{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "effect"
    , "console"
    , "psci-support"
    , "typelevel"
    , "lists"
    , "arrays"
    , "math"
    , "integers"
    , "maybe"
    , "ordered-collections"
    , "unordered-collections"
    , "naturals"
    , "positive-integers"
    , "generics-rep"
    , "strings"
    , "refined"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}