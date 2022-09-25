[![Pursuit documentation](https://img.shields.io/badge/documentation-Pursuit-c43953a)](https://pursuit.purescript.org/packages/purescript-phylio)
![GitHub Workflow Status](https://img.shields.io/github/workflow/status/vibbits/phylio/Test%20CI?style=flat)
![Phylio on Pursuit](https://pursuit.purescript.org/packages/purescript-phylio/badge)
![Phylio on NPM](https://img.shields.io/npm/v/@vibbioinfocore/phylio)

# Phylio
Read, write, and manipulate phylogenetic trees.

Supported formats are:

* [PhyloXML](http://www.phyloxml.org/)
* [Newick](https://en.wikipedia.org/wiki/Newick_format) with support for [NHX](http://www.phylosoft.org/NHX/nhx.pdf) and [Extended Newick](https://doi.org/10.1186/1471-2105-9-532)
* [NEXUS](https://en.wikipedia.org/wiki/Nexus_file)

## Installation

For javascript projects using NPM:
```bash
npm i --save @vibbioinfocore/phylio
```

For javascript projects using yarn:
```bash
yarn add @vibbioinfocore/phylio
```

For Purescript projects:
```bash
spago install purescript-phylio
```

## Usage

From Javascript:
```typescript
import { parse } from '@vibbioinfocore/phylio';

try {
    const tree = parse("(A,B)C;");
} catch (Error e) {
    console.log(e);
}
```

From Purescript:
```haskell
import Bio.Phylogeny (parseNewick)

tree = parseNewick "(A,B)C;"
```

## Contributing

Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update/add tests as appropriate.

## License

[MIT](https://choosealicense.com/licenses/mit/)
