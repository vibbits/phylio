# Phylio
Read, write, and manipulate phylogenetic trees.

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
