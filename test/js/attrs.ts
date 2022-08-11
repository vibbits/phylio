/**
 * Tests for accessing node attributes
 */

import { readFileSync } from "node:fs";

import test from "ava";

import { parsePhyloXml, vertices } from "../../js/main.js";

test("What attributes does PP00079G00800 have?", (t) => {
  const text = readFileSync("test/data/taxonomy.xml", {
    encoding: "utf-8",
  });
  const verts = vertices(parsePhyloXml(text));
  console.log(
    verts.filter((v) => v.name === "PP00079G00800").map((v) => v.attributes)
  );
  t.fail();
});
