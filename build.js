var buildSync = require("esbuild").buildSync;
var Generator = require("npm-dts").Generator;

var dependencies = require("./package.json").dependencies;

var shared = {
  entryPoints: ["js/main.ts"],
  bundle: true,
  minify: true,
  platform: "neutral",
  external: dependencies ? Object.keys(dependencies) : [],
};

buildSync({
  ...shared,
  outfile: "dist/index.js",
});

new Generator({
  entry: "src/main.ts",
  output: "dist/index.d.ts",
}).generate();
