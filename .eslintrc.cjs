module.exports = {
  env: {
    browser: true,
    es2021: true,
    node: true,
  },
  extends: [
    "eslint:recommended",
    "prettier"
  ],
  parser: "@typescript-eslint/parser",
  parserOptions: {
    ecmaVersion: "latest",
    sourceType: "module",
  },
  plugins: ["@typescript-eslint"],
  rules: {
    eqeqeq: ["error", "always"],
    "no-unused-vars": [
      "error",
      {
        argsIgnorePattern: "^_",
      },
    ],
  },
};
