/**
 * JavaScript entrypoint to the Phylio phylogeny library
 */

import {
  parse as parse_,
  //@ts-expect-error This Purescript import should be safe
} from "../output/JsLib/index.js";

declare function parse_(text: string): unknown;

export interface Metadata {
  name: string | undefined;
  parent: number;
  rooted: boolean;
  description: string | undefined;
}

export type Event =
  | "Clade"
  | "Taxa"
  | "Hybrid"
  | "LateralGeneTransfer"
  | "Recombination";

type NumericAttr = { tag: "numeric"; value: number };
type TextAttr = { tag: "text"; value: string };
type BoolAttr = { tag: "bool"; value: boolean };
type ListAttr = { tag: "list"; value: Array<Attribute> };
type MapAttr = { tag: "mapping"; value: Map<string, Attribute> };
export type Attribute = NumericAttr | TextAttr | BoolAttr | ListAttr | MapAttr;

export interface Taxa {
  name: string;
  event: Event;
  branchLength: number;
  ref: number;
  attributes: Map<string, Attribute>;
}

export interface Edge {
  source: number;
  sink: number;
}

export interface Phylogeny {
  metadata: Array<Metadata>;
  nodes: Array<Taxa>;
  edges: Array<Edge>;
}

type Result<T> =
  | { tag: "error"; message: string }
  | { tag: "success"; value: T };

function toPhylogenyResult(val: unknown): Result<Phylogeny> {
  if (val === null || val === undefined) {
    return { tag: "error", message: "Internal error: null" };
  } else {
    if (typeof val === "object" && "tag" in val) {
      if (val.tag == "error" && "message" in val){
        return val as Result<Phylogeny>;
      } else if (val.tag === "success" && "value" in val) {
        return val as Result<Phylogeny>;
      } else {
        return {tag: "error", message: "Internal error: not an expected result"};
      }
    } else {
      return {tag: "error", message: "Internal error: not a result"};
    }
  }
}

export const parse = (text: string): Phylogeny => {
  const res: Result<Phylogeny> = toPhylogenyResult(parse_(text));

  if (res.tag === "error") {
    throw new Error(res.message);
  }

  return res.value;
};

export const edges = (phylogeny: Phylogeny): Array<Edge> => phylogeny.edges;

export const vertices = (phylogeny: Phylogeny): Array<Taxa> => phylogeny.nodes;

export const roots = (phylogeny: Phylogeny): Array<number> =>
  phylogeny.metadata.map((meta) => meta.parent);
