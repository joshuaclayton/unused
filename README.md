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

That said, **be confident the code you're removing won't break your program**.
Especially with projects built in Ruby, Elixir, or JavaScript, there are ways
to dynamically trigger or define behavior that may be surprising. A test suite
can help here, but still cannot determine every possible execution path.

## Installing and Updating

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

To update, run:

```sh
brew update
brew upgrade unused
```

Alternatively, you can install with [Stack] or by hand.

### Stack

If you already have [Stack] installed, ensure you have the latest list of
packages:

```sh
stack update
```

Verify Stack is using at least `lts-6.0` when installing by checking the
global project settings in `~/.stack/global-project/stack.yaml`.

Once that is complete, run:

```sh
stack install unused
```

This will install unused in the appropriate directory for Stack; you'll want
to ensure your `$PATH` reflects this.

### Installing by hand

This project is written in [Haskell] and uses [Stack].

Once you have these tools installed and the project cloned locally:

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

## Troubleshooting

### "Calculating cache fingerprint" takes a long time

`unused` attempts to be intelligent at understanding if your codebase has
changed before running analysis (since it can be time-consuming on large
codebases). To do so, it calculates a "fingerprint" of the entire directory by
using `md5` (or `md5sum`), along with `find` and your `.gitignore` file.

If you're checking in artifacts (e.g. `node_modules/`, `dist/`, `tmp/`, or
similar), `unused` will likely take significantly longer to calculate the
fingerprint.

Per the `--help` documentation, you can disable caching with the `-C` flag:

```sh
$ unused -C
```

### "No results found" when expecting results

If you're expecting to see results but `unused` doesn't find anything, verify
that any artifacts `unused` uses (e.g. the `tags` file, wherever it's located)
or generates (e.g. in `PROJECT_ROOT/tmp/unused`) is `.gitignore`d.

What might be happening is, because unused searches for tokens with `ag`
(which honors `.gitignore`), it's running into checked-in versions of the
tokens from other files, resulting in duplicate occurrences that aren't
representative of the actual codebase. The most obvious might be the `tags`
file itself, although if you're using an IDE that runs any sort of analysis
and that's getting checked in somehow, that may cause it too.

One final piece to check is the number of tokens in the tags file itself; if
`ctags` is misconfigured and only a handful of tokens are being analyzed, they
all may have low removal likelihood and not display in the default results
(high-likelihood only).

## Custom Configuration

The first time you use `unused`, you might see a handful of false positives.
`unused` will look in two additional locations in an attempt to load
additional custom configuration to help improve this.

### Configuration format

```yaml
# Language or framework name
#   e.g. Rails, Ruby, Go, Play
- name: Framework or language
  # Collection of matches allowed to have one occurrence
  autoLowLikelihood:
    # Low likelihood match name
    - name: ActiveModel::Serializer
      # Flag to capture only capitalized names
      #   e.g. would match `ApplicationController`, not `with_comments`
      classOrModule: true

      # Matcher for `.*Serializer$`
      #   e.g. `UserSerializer`, `ProjectSerializer`
      termEndsWith: Serializer

      # Matcher for `^with_.*`
      #   e.g. `with_comments`, `with_previous_payments`
      termStartsWith: with_

      # Matcher for `^ApplicationController$`
      termEquals: ApplicationController

      # Matcher for `.*_factory.ex`
      #   e.g. `lib/appname/user_factory.ex`, `lib/appname/project_factory.ex`
      pathEndsWith: _factory.ex

      # Matcher for `^app/policies.*`
      #   e.g. `app/policies/user_policy.rb`, `app/policies/project_policy.rb`
      pathStartsWith: app/policies

      # list of termEquals
      # Matcher allowing any exact match from a list
      allowedTerms:
      - index?
      - edit?
      - create?
```

### `~/.unused.yml`

The first location is `~/.unused.yml`. This should hold widely-used
configuration roughly applicable across projects. Here's an example of what
might be present:

```yaml
- name: Rails
  autoLowLikelihood:
    - name: ActiveModel::Serializer
      termEndsWith: Serializer
      classOrModule: true
    - name: Pundit
      termEndsWith: Policy
      classOrModule: true
      pathEndsWith: .rb
    - name: Pundit Helpers
      allowedTerms:
        - Scope
        - index?
        - new?
        - create?
        - show?
        - edit?
        - destroy?
        - resolve
    - name: JSONAPI::Resources
      termEndsWith: Resource
      classOrModule: true
      pathStartsWith: app/resources
    - name: JSONAPI::Resources Helpers
      allowedTerms:
      - updatable_fields
      pathStartsWith: app/resources
```

I tend to work on different APIs, and the two libraries I most commonly use
have a fairly similar pattern when it comes to class naming. They both also
use that naming structure to identify serializers automatically, meaning they
very well may only be referenced once in the entire application (when they're
initially defined).

Similarly, with Pundit, an authorization library, naming conventions often
mean only one reference to the class name.

This is a file that might grow, but is focused on widely-used patterns across
codebases. You might even want to check it into your dotfiles.

### `APP_ROOT/.unused.yml`

The second location is `APP_ROOT/.unused.yml`. This is where any
project-specific settings might live. If you're working on a library before
extracting to a gem or package, you might have this configuration take that
into account.

### Validation

`unused` will attempt to parse both of these files, if it finds them. If
either is invalid either due to missing or mistyped keys, an error will be
displayed.

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
