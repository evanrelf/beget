# TODO

- Build events: have the engine emit events (task started, cache hit, cache
  miss, task finished, with keys and timings) to handlers passed to `runBuild`.
  - Build report: a handler that collects which tasks ran, hit the cache, or
    reran because they're volatile, so tests can assert on that instead of
    counting trace rows with SQL.
  - Live logging: a handler, in its own module, that prints running tasks to
    stderr in real time with colors, including why each ran and whether it was
    a cache hit.
- Invalidate the cache when the binary changes: hash the running executable,
  use a random per-session salt in GHCi, and wipe traces when the salt changes.
- Make database setup idempotent (`dbCreate` fails on an existing database).
- `debugTaskCount` counts a build twice if it's cancelled and rerun.
- `$$discoverTasks` needs `IsTask` in scope unqualified, because
  `discoverInstances` looks up the class with `mkName`.
- Show keys as calls (e.g. `fib 29`) in error messages.
- Consider a parallel `Applicative` for `Build`; concurrent `<*>` would record
  both sides in the same cohort.
