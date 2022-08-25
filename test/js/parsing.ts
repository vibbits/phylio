/**
 * Tests for the parsing API in JavaScript
 */

import test from "ava";

import { parse, vertices } from "../../js/main.js";

test("Can parse semicolon phylogeny", (t) => {
  t.deepEqual(vertices(parse(";")), [
    {
      name: "",
      event: "Taxa",
      branchLength: 0.0,
      ref: 0,
      attributes: new Map(),
    },
  ]);
});

test("Can parse empty phylogeny", (t) => {
  t.deepEqual(vertices(parse("();")), [
    {
      name: "",
      event: "Clade",
      branchLength: 0.0,
      ref: 0,
      attributes: new Map(),
    },
    {
      name: "",
      event: "Taxa",
      branchLength: 0.0,
      ref: 1,
      attributes: new Map(),
    },
  ]);
});

test("Parses a phylogeny with names and branch lengths", (t) => {
  t.deepEqual(vertices(parse("(B:0.1)A:1.2;")), [
    {
      name: "A",
      event: "Clade",
      branchLength: 1.2,
      ref: 0,
      attributes: new Map(),
    },
    {
      name: "B",
      event: "Taxa",
      branchLength: 0.1,
      ref: 1,
      attributes: new Map(),
    },
  ]);
});

test("Throws when missing closing paren", (t) => {
  t.throws(() => parse("(;"), { instanceOf: Error });
});

test("Throws when missing opening paren", (t) => {
  t.throws(() => parse(");"), { instanceOf: Error });
});

test("Throws when missing closing semicolon", (t) => {
  t.throws(() => parse("()"), { instanceOf: Error });
});
