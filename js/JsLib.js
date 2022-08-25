export const text = (value) => {
  return { tag: "text", value };
};
export const numeric = (value) => {
  return { tag: "numeric", value };
};
export const bool = (value) => {
  return { tag: "bool", value };
};
export const list = (value) => {
  return { tag: "list", value };
};

// The value here is actually a list of (key, value) pairs
// a Map needs to be constructed.
export const mapping = (value) => {
  let map = new Map();
  value.forEach(([k, v]) => map.set(k, v));
  return { tag: "mapping", value: map };
};

export const mkPair = (a) => (b) => [a, b];

export const mkMap = () => new Map();

export const addKeyToMap = (map) => (key) => (value) => {
  map.set(key, value);
  return map;
};

export const _mkEdge = (source) => (sink) => {
  return { source, sink };
};

export const _mkTaxa = (name, event, branchLength, ref, attributes) => {
  return {
    name,
    event,
    branchLength,
    ref,
    attributes,
  };
};

export const mkError = (message) => {
  return { tag: "error", message };
};

export const mkSuccess = (value) => {
  return { tag: "success", value };
};
