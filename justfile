run:
    cargo run

lint:
    cargo clippy --all --all-features --tests -- -D warnings

lint_fix:
    cargo clippy --all --all-features --tests --fix

build:
    cargo build

test:
    cargo test

test_watch:
    git ls-files | entr cargo test

nestest:
    NESTEST_HACK=1 cargo run > myout.log

pre_commit: lint test build
