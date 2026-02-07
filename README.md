# beget

A monadic, efficient, embedded build system. Features include:

- **Dynamic dependencies:** Dependencies can be discovered at runtime; not
  everything needs to be declared in advance.
- **Early cutoff:** Avoids building reverse dependencies if result hasn't
  changed.
- **Minimal:** Only rebuilds when out-of-date, at most once.
- **Persistent:** Results are cached in a SQLite database, and can be shared
  across machines.
- **Parallel:** Independent builds occur in parallel.
- **Ergonomic:** Tasks can be written as regular Haskell functions, registered
  with Template Haskell.

In ["Build Systems à la Carte"][bsalc] terminology, this pairs a suspending
scheduler with constructive traces for rebuilding, similar to [Buck2][buck2].

This was extracted from [another project][be2], so most of the Git history lives
there.

[bsalc]: https://www.microsoft.com/en-us/research/wp-content/uploads/2018/03/build-systems-final.pdf
[buck2]: https://github.com/facebook/buck2
[be2]: https://github.com/evanrelf/be2
