# Alternative compiler for EkScript written in Zig

## Compile to Zig

This EkScript Compiler compiles to Zig instead of C

- Maintains an excellent C ABI.
- Easier to do coroutines
- Excellent standard library with native support for String hashmap and arraylist
  - Two of the most defining JavaScript data structures

## Why Zig?

- Performant - A faster alternative to the TypeScript compiler
- Custom parser - A faster and more accurate parser that detects errors faster and better
- Better error handling - Better error handling
- Portable: Can be used by anyone

## Building

```sh
git clone https://github.com/ekscript/ekscriptalt
cd ekscriptalt
zig build
```

To run the tests:

```sh
zig build test
```

To run the tests while watching:

```sh
nodemon
```
