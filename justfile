lint:
    cargo clippy --all --all-features --tests -- -D warnings

test_watch:
    git ls-files | entr cargo test

