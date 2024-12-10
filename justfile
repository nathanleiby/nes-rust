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

test_watch:
    git ls-files | entr just test

nestest:
    NESTEST_HACK=1 cargo run roms/nestest.nes > myout.log

pre_commit: lint test build
