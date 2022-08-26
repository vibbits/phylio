/**
 * JavaScript entrypoint to the Phylio phylogeny library
 */

//@ts-ignore - This PureScript import should be safe
import { roots as roots_ } from "../output/Bio.Phylogeny/index.js";

import {
  edges as edges_,
  vertices as vertices_,
  parse as parse_,
  //@ts-ignore - This Purescript import should be safe
} from "../output/JsLib/index.js";

export interface Metadata {
  name: string | undefined;
  parent: number;
  rooted: boolean;
  description: string | undefined;
}

export interface Phylogeny {
  metadata: Array<Metadata>;
  network: unknown;
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

type Result<T> =
  | { tag: "error"; message: string }
  | { tag: "success"; value: T };

export const parse = (text: string): Phylogeny => {
  const res: Result<unknown> = parse_(text);
  if (res.tag === "error") {
    throw new Error(res.message);
  }

  return res.value as Phylogeny;
};

export const edges = (
  graph: unknown,
): Array<{ source: number; sink: number }> => edges_(graph);

export const vertices = (graph: unknown): Array<Taxa> => vertices_(graph);

export const roots = (graph: unknown): Array<number> => roots_(graph);
