let conf = ./spago.dhall

in conf // {
   sources = conf.sources # [ "test/**/*.purs" ],
   dependencies = conf.dependencies # [
   , "aff"
   , "effect"
   , "exceptions"
   , "spec"
   , "node-buffer"
   , "node-fs-aff"
   , "node-path"
   ]
}
