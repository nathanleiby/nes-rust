alias cc := code_coverage
alias pc := pre_commit
alias t := test_all

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

test_all:
    cargo test

test TEST:
    cargo test {{TEST}}

test_watch TEST:
    git ls-files | entr cargo test {{TEST}}

code_coverage:
    cargo tarpaulin -o html && open tarpaulin-report.html

nestest:
    NESTEST_HACK=1 cargo run roms/nestest.nes > myout.log

pre_commit: lint test_all build
