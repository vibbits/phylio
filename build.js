import { buildSync } from "esbuild";
import pkg from "./package.json" assert { type: "json" };

var shared = {
  entryPoints: ["js/main.ts"],
  bundle: true,
  minify: true,
  platform: "neutral",
  external: pkg.dependencies ? Object.keys(pkg.dependencies) : [],
};

buildSync({
  ...shared,
  outfile: "dist/index.js",
});
