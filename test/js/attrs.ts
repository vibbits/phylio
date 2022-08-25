/**
 * Tests for accessing node attributes
 */

import { readFileSync } from "node:fs";

import test from "ava";

import { parse, vertices, Attribute, Taxa } from "../../js/main.js";

test("What attributes does PP00079G00800 have?", (t) => {
  const text = readFileSync("test/data/taxonomy.xml", {
    encoding: "utf-8",
  });
  const verts = vertices(parse(text));
  const PP00079G00800 = verts.filter(
    (v) => v.name === "PP00079G00800"
  )[0] as Taxa;
  t.deepEqual(Array.from(PP00079G00800.attributes.keys()), [
    "taxonomy",
    "sequence",
    "property",
  ]);
  const taxonomy = PP00079G00800.attributes.get("taxonomy");
  t.is(taxonomy?.tag, "mapping");
  t.deepEqual(
    taxonomy?.value,
    new Map([["code", { tag: "text", value: "ppa" }]])
  );

  const property = PP00079G00800.attributes.get("property");
  t.is(property?.tag, "mapping");
  t.deepEqual(
    property?.value,
    new Map([
      ["datatype", { tag: "text", value: "xsd:anyURI" }],
      ["applies_to", { tag: "text", value: "clade" }],
      ["ref", { tag: "text", value: "See also" }],
      [
        "value",
        {
          tag: "text",
          value:
            "http://bioinformatics.psb.ugent.be/plaza/versions/plaza_v3_dicots/genes/view/PP00079G00800",
        },
      ],
    ])
  );
  const sequence = PP00079G00800.attributes.get("sequence");
  t.is(sequence?.tag, "mapping");
  const domarch = (sequence?.value as Map<string, Attribute>).get(
    "domain_architecture"
  );
  t.deepEqual((domarch?.value as Map<string, Attribute>).get("length"), {
    tag: "numeric",
    value: 2214,
  });
  t.deepEqual(
    (
      (domarch?.value as Map<string, Attribute>).get("domain")
        ?.value as Array<Attribute>
    )[8],
    {
      tag: "mapping",
      value: new Map([
        ["from", { tag: "numeric", value: 135 }],
        ["confidence", { tag: "numeric", value: 24.271 }],
        ["to", { tag: "numeric", value: 540 }],
        ["value", { tag: "text", value: "IPR017932" }],
      ]),
    }
  );
});

test("Empty attribute", (t) => {
  const text = readFileSync("test/data/attrs.xml", {
    encoding: "utf-8",
  });
  const node = vertices(parse(text))[0] as Taxa;
  const empty = node.attributes.get("empty");
  t.is(empty?.tag, "mapping");
  t.deepEqual(empty?.value, new Map());
});

test("Duplicated attributes", (t) => {
  t.plan(5);
  const text = readFileSync("test/data/attrs.xml", {
    encoding: "utf-8",
  });
  const node = vertices(parse(text))[0] as Taxa;
  const textAttr = node.attributes.get("textAttr");
  t.is(textAttr?.tag, "list");
  (textAttr?.value as Array<Attribute>).forEach((a) => {
    t.is(a.tag, "mapping");
  });

  t.deepEqual(
    (textAttr?.value as Array<Attribute>)[0]?.value,
    new Map([
      ["textA", { tag: "text", value: "test 1" }],
      ["textB", { tag: "text", value: "test 2" }],
    ])
  );

  t.deepEqual(
    (textAttr?.value as Array<Attribute>)[1]?.value,
    new Map([
      ["textA", { tag: "text", value: "test" }],
      ["textB", { tag: "text", value: "another" }],
    ])
  );
});

test("More duplicated attributes", (t) => {
  t.plan(5);
  const text = readFileSync("test/data/attrs.xml", {
    encoding: "utf-8",
  });
  const node = vertices(parse(text))[0] as Taxa;
  const numericAttr = node.attributes.get("numericAttr");
  t.is(numericAttr?.tag, "list");
  (numericAttr?.value as Array<Attribute>).forEach((a) => {
    t.is(a.tag, "mapping");
  });

  t.deepEqual(
    (numericAttr?.value as Array<Attribute>)[0]?.value,
    new Map([["num", { tag: "numeric", value: 123 }]])
  );

  t.deepEqual(
    (numericAttr?.value as Array<Attribute>)[1]?.value,
    new Map([["num", { tag: "numeric", value: 123 }]])
  );
});
