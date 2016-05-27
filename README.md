# Unused [![Build Status](https://travis-ci.org/joshuaclayton/unused.svg?branch=master)](https://travis-ci.org/joshuaclayton/unused)

A command line tool to identify unused code.

![Image of Unused Output](http://i.giphy.com/3o7qDT1I4OfQxnJTvW.gif)

## "What kinds of projects can I used it on?"

Anything.

Yes, literally anything.

It's probably best if you have a ctags file it can read from (it looks in
`.git`, `tmp`, and the root directory for a `tags` file), but if you have
another way to pipe a bunch of methods/functions/classes/modules/whatever in,
that works too.

Right now, there are some special cases built in for Rails and Phoenix apps
(specifically, assumptions about what's fine to only have one reference to,
e.g. Controllers in Rails and Views in Phoenix), but it'll work on Rubygems,
Elixir packages, or anything else.

That said, be confident the code you're removing won't break your program.
Especially with projects built in Ruby, Elixir, or JavaScript, there are ways
to dynamically trigger or define behavior that may be surprising. A test suite
can help here, but still cannot determine every possible execution path.

## Installing

### Homebrew (Recommended)

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

### Installing by hand

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

`unused` attempts to read from common tags file locations (`.git/tags`,
`tags`, and `tmp/tags`).

In an application where the tags file exists, run:

```sh
unused
```

If you want to specify a custom tags file, or load tokens from somewhere else,
run:

```sh
cat .custom/tags | unused --stdin
```

To view more usage options, run:

```sh
unused --help
```

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
