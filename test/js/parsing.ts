/**
 * Tests for the parsing API in JavaScript
 */

import test from "ava";

import { parseNewick, vertices } from "../../js/main.js";

test("Can parse semicolon phylogeny", (t) => {
  t.deepEqual(vertices(parseNewick(";")), [
    {
      name: "",
      node: "Taxa",
      branchLength: 0.0,
      ref: 0,
      attributes: new Map(),
    },
  ]);
});

test("Can parse empty phylogeny", (t) => {
  t.deepEqual(vertices(parseNewick("();")), [
    {
      name: "",
      node: "Clade",
      branchLength: 0.0,
      ref: 0,
      attributes: new Map(),
    },
    {
      name: "",
      node: "Taxa",
      branchLength: 0.0,
      ref: 1,
      attributes: new Map(),
    },
  ]);
});

test("Parses a phylogeny with names and branch lengths", (t) => {
  t.deepEqual(vertices(parseNewick("(B:0.1)A:1.2;")), [
    {
      name: "A",
      node: "Clade",
      branchLength: 1.2,
      ref: 0,
      attributes: new Map(),
    },
    {
      name: "B",
      node: "Taxa",
      branchLength: 0.1,
      ref: 1,
      attributes: new Map(),
    },
  ]);
});

test("Throws when missing closing paren", (t) => {
  t.throws(() => parseNewick("(;"), { instanceOf: Error });
});

test("Throws when missing opening paren", (t) => {
  t.throws(() => parseNewick(");"), { instanceOf: Error });
});

test("Throws when missing closing semicolon", (t) => {
  t.throws(() => parseNewick("()"), { instanceOf: Error });
});
