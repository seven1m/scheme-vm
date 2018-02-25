![no idea](https://i.imgur.com/e7ArY2r.jpg)

[![Circle CI](https://circleci.com/gh/seven1m/scheme-vm.svg?style=svg)](https://circleci.com/gh/seven1m/scheme-vm)

_a work-in-progress R7RS scheme implementation in Ruby & Rust for my own amusement_

[the todo list](https://github.com/seven1m/scheme-vm/issues/12)

## Build

Requirements:

* Rust 1.24.0 or better
* Ruby 2.4.0 or better
  * Bundler gem (`gem install bundler`)

```
cargo build
bundle install
```

## Tests

```
bundle exec rspec
```

## Run

```
bin/scheme examples/fib.scm
```

## License

Copyright Tim Morgan. Licensed MIT.
