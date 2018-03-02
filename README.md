![no idea](https://i.imgur.com/e7ArY2r.jpg)

[![Circle CI](https://circleci.com/gh/seven1m/scheme-vm.svg?style=svg)](https://circleci.com/gh/seven1m/scheme-vm)

_a work-in-progress R7RS scheme implementation in Ruby & Rust for my own amusement_

[the todo list](https://github.com/seven1m/scheme-vm/issues/12)

## Build

_requires Ruby and Rust 1.24.0 or better_

Assuming you have [rbenv](https://github.com/rbenv/rbenv):

```
git clone https://github.com/seven1m/scheme-vm
cd scheme-vm
CONFIGURE_OPTS=--enable-shared rbenv install
RUBY=$(rbenv which ruby) cargo build
gem install bundler
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
