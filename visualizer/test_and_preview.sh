#!/usr/bin/env sh
cargo test $1 | tee >(grep -oP "result: \[.+?\]" | ./show_test_layout.py) || gwenview test_layout.svg &> /dev/null

