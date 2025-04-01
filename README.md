# `tromp`

`tromp` is an OCaml-written tool that generates Tromp diagrams of $\lambda$-expressions!

## Building

1. Run `opam exec -- dune build` (this requires you to have both `opam` and `dune` installed; the latter can be installed using the former).
2. Locate the executable: `_build/install/default/bin/tromp`.

Run `tromp --help` for an overview of the tool's usage. Right now, the only supported rendering mode is `text`.

## Formal grammar of $\lambda$-expressions

* Any alphanumeric identifier with underscores, hyphens, and apostrophes, is a variable.
  * Examples: `x`, `x_with-UndeRsc0res_hyphens'''and___apostrophes`, `128WITCHCHICK`.
* **Variable**. Any variable is a well-formed $\lambda$-expression.
* **Application**. If `lhs` and `rhs` are well-formed $\lambda$-expressions, so is `(lhs rhs)`.
  * Application is *left-associative*. Parentheses can be omitted if left associativity is used accordingly.
  * Whitespaces separating `lhs` and `rhs` are *mandatory*.
* **Abstraction**. If `x` is a variable and `e` is a well-formed $\lambda$-expression, so is `(\ x . e)`.
  * Abstraction is *right-associative*. Parentheses can be omitted if the lambda intends to consume everything to its right.
  * Back-to-back binders can be stacked: `\ x . \ y . \ z . E` can be rewritten compactly as `\ x y z . E`.
  * Whitespaces separating `x`, `e`, and the other syntactical elements, are *optional*.

## A note on free variables

This tool *permits* free variables in your $\lambda$-terms, so they need not be combinators! In the diagrams they are repressented by spikes that shoot up. One can imagine them connecting to some binder outside of the given $\lambda$-term.

## Roadmap (WIP)

- [x] Construct a text-rendering algorithm w/ Unicode code points.
- [ ] Construct an image-rendering algorithm.
- [ ] Add 'λ', 'fu?n', and '->' to the CFG of $\lambda$-expressions.
- [ ] Generate a series of images, where each next image is the current image β-reduced once, with the option to specify the number of images.
- [ ] Generate a GIF, animating β-reduction of a $\lambda$-expression, with the option to specify the delay and the number of frames.

## Acknowledgements

- This project was inspired by 2swap's video on $\lambda$-calculus and Tromp diagrams called [What is PLUS times PLUS?](https://youtu.be/RcVA8Nj6HEo?si=jWpPof803_kHkxUP)
- John Tromp's [home page](https://tromp.github.io/).
  - In particular, [Lambda Diagrams](https://tromp.github.io/cl/diagrams.html).

### Tool naming

The name of this tool is subject to change. Even though `tromp` is concise and easy to type, associating the diagram generation purely with John Tromp's last name shadows his substantial work in other areas.

If you have any suggestions, feel free to create a ticket and discuss with me the tool's naming there!
