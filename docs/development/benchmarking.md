# Benchmarking

kon has two layers of performance measurement. Go benchmarks guard individual
hot paths and run anywhere. The load test drives the real binary end to end, so
it catches costs that no single package sees, such as a per-delta copy in the
provider that the UI benchmarks cannot reach.

## Go benchmarks

```sh
make bench
```

This runs every `Benchmark` in the module with allocation counts. The UI and
markdown suites dominate the run time, so filter while iterating:

```sh
go test -run '^$' -bench DecodeChatStream -benchmem ./internal/provider
```

| Benchmark | Guards |
| --- | --- |
| `internal/provider` `BenchmarkDecodeChatStream` | SSE decoding stays flat per delta as a reply grows |
| `internal/markdown` `BenchmarkStream*`, `BenchmarkRender*` | streamed markdown rendering |
| `internal/ui` `BenchmarkTranscriptRender*`, `BenchmarkViewportRefresh` | transcript folding and per-frame refresh; see [rendering-performance.md](rendering-performance.md) |
| `internal/catalog` `BenchmarkNew` | loading the bundled model catalog |

Several stream benchmarks grow their input on every iteration, so their
`ns/op` depends on `b.N` and is not comparable across machines or runs. Compare
them before and after a change on the same machine, or report a per-unit metric
as `BenchmarkDecodeChatStream` does with `ns/delta`.

## Load test

```sh
make loadtest        # headless scenarios
make loadtest TUI=1  # also the full-screen UI, which needs Linux and tmux
```

`scripts/loadtest` builds nothing itself; the make target builds `bin/kon`
first. It serves a mock OpenAI-compatible model in process, points an isolated
config and data directory at it, and runs fixed scenarios. Pass `-run <regexp>`
to `go run ./scripts/loadtest` to select scenarios and `-kon <path>` to measure
another binary, such as a release build, against the same load.

- **Headless** scenarios run `kon run` to completion and report wall time, CPU
  time split into user and sys, and peak RSS from the child's rusage.
- **TUI** scenarios start kon in a private tmux server, send one prompt, and
  sample `/proc` once a second. They report the CPU percent of each second, so a
  per-frame cost that grows with the reply shows up as a rising row rather than
  being averaged away.

The mock streams three reply shapes. `doc` separates markdown blocks with blank
lines like typical model output. `paragraph` never breaks a line, and `code` is
one fence that never closes; neither gives the markdown stream a blank line to
settle at, so they are the worst case for streamed rendering. Tool scenarios
have the model call `shell` for a set number of turns before replying.

### Baseline

Intel i5-8500T, 4 cores, Linux:

| Scenario | Wall | CPU | Peak RSS | Notes |
| --- | ---: | ---: | ---: | --- |
| `startup` | 21 ms | 9 ms | 10.9 MB | one short reply, including process start |
| `stream-doc-50k` | 0.67 s | 0.64 s | 17.8 MB | 50k deltas as fast as the socket allows |
| `stream-doc-200k` | 2.4 s | 2.3 s | 26.8 MB | linear in reply length |
| `stream-code-50k` | 0.64 s | 0.59 s | 17.7 MB | |
| `tools-100` | 1.9 s | 0.93 s | 27.8 MB | 100 sequential shell calls |
| `tools-25x8` | 2.1 s | 1.06 s | 35.1 MB | 8 parallel calls per turn |
| `tools-bigout-30` | 1.1 s | 0.75 s | 30.3 MB | each call prints 1.3 MB |
| `tui-idle` | 5 s | 1% | 31 MB | |
| `tui-doc` | 15 s | 12%, flat | 35 MB | ~100 deltas/s |
| `tui-burst` | 15 s | 9%, flat | 36 MB | the same rate, five deltas per 50 ms |
| `tui-paragraph` | 15 s | 11% rising to 31% | 36 MB | tail re-parse grows with the reply |
| `tui-code` | 15 s | 11% rising to 21% | 35 MB | same, inside one fence |
| `tui-tools` | 8 s | up to 130% while running | 52 MB | 100 tool turns, then idle |

Startup was 50 ms until `go-runewidth` v0.0.30, whose predecessors built a
width table for every Unicode code point in their package `init`; check
`GODEBUG=inittrace=1 bin/kon --version` when a dependency changes.

Before `BenchmarkDecodeChatStream` existed, the provider grew each streamed
part with `+=`, so a 200k-delta reply took 46 s instead of 2.4 s. The rising
`tui-paragraph` and `tui-code` rows are the known cost of re-parsing the
unsettled tail described in `internal/markdown`'s `Stream`.
