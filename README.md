# Rust Interpreter

I'm playing around with interpreters and compilers. This repo is those attempts
over time. I started referencing [this series][1], but hit some issues so
switched instead [to this one][2].

Additional reference that I dug up when I got stuck on walking the AST with the
Rust primitives I wanted to use:

* https://github.com/rust-unofficial/patterns/blob/master/patterns/visitor.md
* https://github.com/rust-lang/rust/blob/master/src/libsyntax/visit.rs
* http://thume.ca/2019/04/18/writing-a-compiler-in-rust/
* http://nedellis.com/2019/05/08/esta_1/
* https://gitlab.com/jrop/rust-calc/blob/master/src/ast.rs

I'll continue to add links to any useful resources I encounter while going
through this but may miss some when I'm going fast.

## Contributing

If you want to comment or see any issues with how I'm doing anything feel free
to open a PR and we can discuss. I don't plan on accepting outside code
contributions, but in the event I change my mind contributions will require
being licensed under the same license as the repository and I reserve the right
to change the license in the future (including that covering any
contributions). By opening a PR you agree to these terms.

[1]: https://ruslanspivak.com/lsbasi-part1/
[2]: https://craftinginterpreters.com/
