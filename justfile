lint:
    cargo clippy --all --all-features --tests -- -D warnings

build:
    cargo build

test:
    cargo test

test_watch:
    git ls-files | entr cargo test

pre_commit: lint test build
