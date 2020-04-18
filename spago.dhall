{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "prelude", "effect", "console", "psci-support", "refs", "ordered-collections", "newtype", "web-dom", "web-html", "js-timers" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
