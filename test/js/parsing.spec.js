/**
 * Tests for the parsing API in JavaScript
 */

import {
  parseNewick,
  parseNexus,
  parsePhyloXml,
  vertices,
} from "../../js/main.ts";

const tree = parseNewick("(B:0.1)A:1.2;");
describe("Parses Newick phylogenies", () => {
  it("simple", () => {
    expect(vertices(tree)).toEqual([
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
});
