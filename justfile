alias cc := code_coverage

run:
    cargo run

lint:
    cargo clippy --all --all-features --tests -- -D warnings

lint_watch:
    git ls-files | entr just lint

lint_fix:
    cargo clippy --all --all-features --tests --fix

build:
    cargo build

test:
    cargo test

test_watch_all:
    git ls-files | entr cargo test

test_watch TEST:
    git ls-files | entr cargo test {{TEST}}::tests


code_coverage:
    cargo tarpaulin -o html

nestest:
    NESTEST_HACK=1 cargo run roms/nestest.nes > myout.log

pre_commit: lint test build
