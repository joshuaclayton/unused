# Unused

A command line tool in Haskell to identify unused code.

## Using Unused

`unused` reads from a pipe expecting a series of tokens to search the codebase
for.

This could be done with `echo`:

```sh
echo "module\nimport" | unused
```

Or pulling in a ctags file:

```sh
cat .git/tags | cut -f1 | sort -u | unused
```

My end goal is to have the latter rolled up into unused itself, so you can
navigate to a directory, run `unused`, and everything works as expected.

## Building and Compiling

This project uses Haskell and Stack.

Once you have these tools installed:

```sh
stack setup
stack build
```

This will generate a binary in `.stack-work/install/**/**/**/bin` that you'll
want to move to somewhere in your `$PATH`.

## License

Copyright 2016 Josh Clayton. See the [LICENSE](LICENSE).
