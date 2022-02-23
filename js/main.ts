/**
 * JavaScript entrypoint to the Phylio phylogeny library
 */

//@ts-ignore - This PureScript import should be safe
import * as Internal from "../output/Bio.Phylogeny/index.js";
//@ts-ignore - This PureScript import should be safe
import { either } from "../output/Data.Either/index.js";
//@ts-ignore - This PureScript import should be safe
import { fromMaybe } from "../output/Data.Maybe/index.js";
//@ts-ignore - This PureScript import should be safe
import { fst, snd } from "../output/Data.Tuple/index.js";

interface Metadata {
  name: string | undefined;
  parent: number;
  rooted: boolean;
  description: string | undefined;
}

interface Phylogeny {
  metadata: Array<Metadata>;
  network: unknown;
}

type NodeType =
  | "Clade"
  | "Taxa"
  | "Hybrid"
  | "LateralGeneTransfer"
  | "Recombination";

type NumericAttr = { tag: "numeric"; value: number };
type TextAttr = { tag: "text"; value: string };
type BoolAttr = { tag: "bool"; value: boolean };
type Attribute = NumericAttr | TextAttr | BoolAttr;

interface Taxa {
  name: string;
  node: NodeType;
  branchLength: number;
  ref: number;
  attributes: Map<string, Attribute>;
}

const reportError =
  (text: string) =>
  (err: unknown): string =>
    Internal.reportError(err)(text);

export const parseNewick = (text: string): Phylogeny | string =>
  either(reportError(text))((x: Phylogeny) => x)(Internal.parseNewick(text));

export const parseNexus = (text: string): Phylogeny | string =>
  either(reportError(text))((x: Phylogeny) => x)(Internal.parseNexus(text));

export const parsePhyloXml = (text: string): Phylogeny | string =>
  either(reportError(text))((x: Phylogeny) => x)(Internal.parsePhyloXml(text));

const attrsToMap = (attrs: unknown): Map<string, Attribute> => {
  const convert = {
    text: (key: string) => (val: string) => [key, { tag: "text", value: val }],
    numeric: (key: string) => (val: number) =>
      [key, { tag: "numeric", value: val }],
    bool: (key: string) => (val: boolean) => [key, { tag: "bool", value: val }],
  };
  return Internal.attrsToForeign(convert)(
    (acc: Map<string, Attribute>) =>
      ([key, value]: [string, Attribute]) =>
        acc.set(key, value)
  )(new Map())(attrs);
};

const toTaxa = (taxa: unknown): Taxa | undefined => {
  const ref: number | undefined = fromMaybe(undefined)(
    (taxa as { ref: unknown }).ref
  );
  if (ref === undefined) {
    return undefined;
  } else {
    return {
      name: (taxa as { name: string }).name,
      node: Internal.nodeTypeToString((taxa as { node: unknown }).node),
      branchLength: (taxa as { branchLength: number }).branchLength,
      ref: ref,
      attributes: attrsToMap((taxa as { attributes: unknown }).attributes),
    };
  }
};

export const edges = (graph: unknown): Array<{ from: Taxa; to: Taxa }> => {
  return Internal.edges(graph).map((edge: unknown) => {
    return { from: toTaxa(fst(edge)), to: toTaxa(snd(edge)) };
  });
};

export const vertices = (graph: unknown) =>
  Internal.vertices(graph).map(toTaxa);

export const traverse = (
  fn: (_taxa: Taxa, _children: Array<number>) => Taxa,
  phylogeny: Phylogeny
): Phylogeny => {
  const thefn = (t: unknown) => (o: Array<number>) => {
    const theTaxa = toTaxa(t);
    if (theTaxa) {
      fn(theTaxa, o);
    }
  };
  return Internal.traverseNetwork(thefn)(phylogeny);
};
