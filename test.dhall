let config = ./spago.dhall
in config // { sources = config.sources # [ "test/**/*.purs" ]
             , dependencies = [ "console", "spec" ]
             }
