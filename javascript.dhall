let conf = ./spago.dhall

in conf // {
   sources = conf.sources # [ "js/**/*.purs" ],
   dependencies = conf.dependencies # [
   , "foreign"
   , "functions"
   ]
}
