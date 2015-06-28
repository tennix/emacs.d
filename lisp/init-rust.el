(require-package 'rust-mode)

(setq racer-rust-src-path "~/Projects/rust/rustc-1.0.0/src/")
(setq racer-cmd "~/Projects/rust/racer/target/release/racer")
(add-to-list 'load-path "~/Projects/rust/racer/editors")
(eval-after-load "rust-mode" '(require 'racer))

(provide 'init-rust)
