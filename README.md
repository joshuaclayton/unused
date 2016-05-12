# Unused

A command line tool in Haskell to identify unused code.

![Image of Unused Output](http://i.giphy.com/3oEjHGgyV2EDdy1Ogw.gif)

## Installing

### Homebrew

You can install [my formulae] via [Homebrew] with `brew tap`:

```sh
brew tap joshuaclayton/formulae
```

Next, run:

```sh
brew install unused
```

[my formulae]: https://github.com/joshuaclayton/homebrew-formulae
[Homebrew]: http://brew.sh/

Alternatively, you can install by hand.

### Stack

This project uses [Haskell] and [Stack].

Once you have these tools installed:

```sh
stack setup
stack install
```

This will generate a binary in `$HOME/.local/bin`; ensure this directory is in
your `$PATH`.

[Haskell]: https://www.haskell.org
[Stack]: http://www.haskellstack.org

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

## Requirements

Unused leverages [Ag](https://github.com/ggreer/the_silver_searcher) to
analyze the codebase; as such, you'll need to have `ag` available in your
`$PATH`. This is set as an explicit dependency in Homebrew.

## Testing

To run the test suite, run:

```sh
stack test
```

## License

Copyright 2016 Josh Clayton. See the [LICENSE](LICENSE).
