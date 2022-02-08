/**
 * JavaScript entrypoint to the PhylIO phylogeny library
 */

import * as Internal from "../dce-output/Bio.Phylogeny/index.js";
import { either } from "../dce-output/Data.Either/index.js";

const reportError = (text: string) => (err) => Internal.reportError(err)(text);

export const parseNewick = (text: string) =>
  either(reportError(text))((x) => x)(Internal.parseNewick(text));

export const parseNexus = (text: string) =>
  either(reportError(text))((x) => x)(Internal.parseNexus(text));

export const parsePhyloXml = (text: string) =>
  either(reportError(text))((x) => x)(Internal.parsePhyloXml(text));
