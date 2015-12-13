# simply-lf-easier


## Introduction

An implementation of the polymorphic lambda calculus for Lisp Flavoured Erlang. I greatly enjoy the benefits that types and their abstractions provide. So I guess it was inevitable that this sort of madness would spring forth. Not sure how far I will push this or if it's even sensible; It is awkward to include types in a Lisp. Erlang is notoriously difficult to effectively provide types for, not impossible, just difficult.

Wish me luck? Thoughts and suggestions encouraged using issues/PRs/nagging on IRC. I'm _mankyKitty_ on freenode.net in #erlang-lisp.

## Installation

Just add it to your ``rebar.config`` deps:

```erlang
  {deps, [
    ...
    {simply-lf-easier, ".*",
      {git, "git@github.com:YOURNAME/simply-lf-easier.git", "master"}}
      ]}.
```

And then do the usual:

```bash
    $ rebar get-deps
    $ rebar compile
```


## Usage

I have no idea how to integrate or utilise this correctly yet... Watch this space?
