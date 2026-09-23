# Transcript rendering performance

The TUI slows down as a session grows. This document records the measured cause
and the phased plan to fix it.

## Symptom

Rendering cost grows with the total number of transcript blocks. The UI stays
responsive early in a session and degrades as history accumulates, most visibly
while a stream is open or a tool-heavy turn emits structural events.

## Measured baseline

Numbers below are from `internal/ui` benchmarks on an Intel i5-8500T, using
representative blocks (user and assistant messages of ~500 bytes and grouped
tool calls). The pre-fix measurements are the from-scratch reference now kept as
`BenchmarkTranscriptRenderFull`.

| blocks | full rebuild | refresh + `viewport.View` |
| -----: | -----------: | ------------------------: |
|    100 |       6.9 ms |                   5.6 ms |
|    500 |      35.8 ms |                  25.5 ms |
|   2000 |       141 ms |                    99 ms |

With `streamFrameInterval = 50ms` (20 fps) and a tool-heavy run emitting a
structural event per tool call, a 2000-block session needs ~296 ms per repaint,
far past the 50 ms frame budget. Cost is linear in history size, so the
degradation is gradual and "fast at first, grinds later."

## Causes

1. **Every structural event re-rendered the whole transcript.** A block append
   marked the transcript dirty and `render` rebuilt every block from scratch.
2. **The viewport rescans the whole document several times per frame.**
   `SetContent` splits and measures all lines, `AtBottom` calls `maxYOffset`,
   and `GotoBottom`/`View` call `calculateLine` again. Soft wrap makes each of
   those a full O(n) pass.
3. **The live stream was re-rendered from scratch every frame.** `pending`
   re-wrapped the entire accumulated stream on each 50 ms tick, which is O(n^2)
   across one long streamed message.
4. **The viewport styles the whole visible slice** on every frame.

## On moving streaming to another thread

Events already arrive on a background goroutine and are coalesced by the render
frame. The cost is in formatting and drawing on the Bubble Tea update goroutine,
not in receiving events, and a Bubble Tea `Model` must not be mutated from
another goroutine. A naive worker thread would move the same O(n) work. The
useful form of that idea is a render worker that owns an immutable line cache
and hands the model snapshots, which is kept as an optional later phase. Phases
1 and 2 remove the bulk of the cost without added concurrency.

## Phase 1 — Incremental transcript rendering (implemented)

Replace the all-or-nothing cached string with a chunk cache that folds only
newly stable blocks.

- `transcript` keeps `chunks []string` (rendered text of stable blocks) and
  `built int` (how many blocks are folded), plus a memoized `joined` string.
- `ensureChunks` folds only blocks before the trailing run of tool/result
  blocks. A run in progress renders live from the unfolded tail and is
  re-rendered only while it is the active run.
- Appending either extends the trailing run in place or terminates it, so the
  stable boundary never rewinds and chunks are never folded twice.
- A width change invalidates the cache once and rebuilds.
- Regression tests assert the incremental output equals a from-scratch render
  at every step, across block kinds and width changes.

Result (render after one new block, warm-cache fold + rejoin):

| blocks | before |     after |
| -----: | -----: | --------: |
|    100 | 6.9 ms | **37 µs** |
|    500 | 35.8 ms | **124 µs** |
|   2000 |  141 ms | **299 µs** |

The dominant remaining cost is re-joining the stable chunks into one string,
which copies the whole history once per structural event (67 allocs, ~1.4 MB at
2000 blocks). The cost is therefore O(history bytes), not O(1) — an earlier
revision of this table claimed 22–27 ns, but that benchmark reused a warm
transcript whose fold state made every iteration after the first a no-op. The
measured cost is still 400–2000× under the from-scratch rebuild and far inside
the 50 ms frame budget; removing the rejoin (a line-slice cache instead of a
joined string) is a possible follow-up.

Guarding benchmarks live in `internal/ui/bench_test.go`:
`BenchmarkTranscriptRenderAppend`, `BenchmarkTranscriptRenderStream`,
`BenchmarkTranscriptRenderFull` (reference), and `BenchmarkViewportRefresh`.

## Phase 2 — Stop the viewport from rescanning the document (implemented)

- The transcript now produces a flat slice of display lines
  (`transcript.linesFor`) and the model feeds it to `viewport.SetContentLines`
  instead of building and re-splitting a joined string.
- `linesFor` keeps a line cache across frames. While only the live stream
  changes it truncates to the cached stable prefix and re-splits just the
  separator and the live tail, so a repaint no longer walks the whole history.
  `prepare` separates the chunk folding/joining from assembly so the joined
  stable text is only rebuilt when the chunk set actually changes.
- The transcript already hard-wraps every line to the viewport width, so the
  viewport's soft wrap is turned off. With `SoftWrap` off its `calculateLine`,
  `maxYOffset`, `AtBottom`, and `GotoBottom` are O(1) instead of O(n), removing
  the repeated full-document wrap scans.

Interim result (per-frame refresh + `viewport.View`):

| blocks | before |     after |
| -----: | -----: | --------: |
|    100 | 5.6 ms | **2.0 ms** |
|    500 | 25.5 ms | **7.2 ms** |
|   2000 |  99 ms | **27 ms** |

The remaining 2000-block cost was dominated by `SetContentLines`, which still
scanned and measured every line with grapheme-cluster segmentation
(`ansi.StringWidth`) on each frame. That is the target of Phase 2b.

Regression test: `TestLineCacheMatchesRenderedLines` asserts the cached lines
equal a from-scratch render after every append, stream delta, and finish.

## Phase 2b — Replace the viewport with a line-slicing scroll container (implemented)

Profiling showed ~80% of the remaining frame was `viewport.SetContentLines` →
`maxLineWidth` → `ansi.StringWidth`, which does full grapheme-cluster
segmentation over every line per frame to compute the longest line width — a
value the transcript never needs because it already wraps every line to the
viewport width.

- `scrollView` (`internal/ui/scroll.go`) replaces `bubbles/viewport`. It holds
  the display lines as-is, keeps a `yOffset`, and its `AtBottom`/`GotoBottom`/
  `maxYOffset` are pure integer arithmetic. `View` slices `lines[yOffset:yOffset+height]`
  and pads to height, so no line is measured.
- `SetContentLines` only stores the slice and clamps the offset; the transcript's
  line cache is passed straight through with no copy.
- Keyboard paging stays in `handleKey` (PgUp/PgDn) and the wheel is handled in
  `scrollView.Update`, preserving the "plain keys go to the prompt" behavior.

Result (per-frame refresh + `scrollView.View`):

| blocks | before |       after |
| -----: | -----: | ----------: |
|    100 |  2.0 ms | **11 µs** |
|    500 |  7.2 ms | **11 µs** |
|   2000 |   27 ms | **11 µs** |

The per-frame refresh is now 29 allocations and ~1 KB regardless of history
size. Regression tests live in `internal/ui/scroll_test.go` (clamping, paging,
wheel, fixed-height render, shrink).

## Phase 3 — Tail-only streaming render (implemented)

`pending` re-wrapped the entire accumulated stream every frame, so a single long
streamed message cost O(n) per frame and O(n^2) across the stream.

- `plainWrapper` (`internal/ui/wrap.go`) is a streaming word wrapper that mirrors
  `lipgloss.Wrap` with no breakpoints byte-for-byte (guarded by
  `TestPlainWrapperMatchesLipgloss`). Its `Finalized()` lines are append-only and
  `Current()` returns the single still-growing line, so `Write` can be fed
  deltas and only the tail is ever revised.
- `liveStream` (`internal/ui/live.go`) folds deltas through a streaming
  normalizer (equivalent to `normalizeText`) into the wrapper and paints the
  finished lines once. Per frame it only repaints the current line.
- `messageSlab` and `thinkingLines` now share `linePainter` and `wrapPlain`, so
  a message rendered live and later folded into a stable block is byte-identical
  (guarded by `TestStreamRenderMatchesFullRender` across widths and chunk sizes).
- `transcript.linesFor` keeps the stable prefix and the live stream's finalized
  lines across frames, dropping and rebuilding only the current line.

Result (repaint while streaming, `BenchmarkTranscriptRenderStream`):

| blocks | before (Phase 2b) |     after |
| -----: | ----------------: | --------: |
|    100 |           91 µs | **11.1 µs** |
|    500 |          211 µs | **10.9 µs** |
|   2000 |          465 µs | **10.9 µs** |

The cost is now flat in block count. For one growing message
(`BenchmarkTranscriptRenderLongStream`), repaints hold ~11 µs whether the
message has 1k or 10k words.

Two costs outside the wrapper were found and fixed while measuring this:

- The stream buffer itself was accumulated with `+=`, re-copying the whole
  message on every delta — O(n²) across the stream, and the dominant term
  once a message reached a few hundred KB. It is an append-only `[]byte` now.
- Stream deltas pass through `ansiStripper` before the wrapper, because
  `plainWrapper` treats escape bytes as printable word text and would split a
  sequence mid-escape. The stripper is stateful, so a sequence split across
  deltas is dropped whole; stripping happens at the transcript boundary so the
  buffered text and the live render always see identical bytes.

## Phase 4 — Render worker (optional)

If phases 1-3 are insufficient, add a goroutine that owns the transcript and
produces immutable snapshots. The model sends it events and receives
"snapshot ready" messages, which is the concurrency-safe form of moving
streaming off the update goroutine.

## Phase 5 — Keep the banner out of the frame's change key (implemented)

Phases 2b/3 claimed a flat per-frame refresh, but the guarding benchmark
(`BenchmarkViewportRefresh`) built transcripts with no banner, while production
transcripts always carry the welcome mark. That hid an O(history) per-frame cost:
`stableBase` returned `banner + "\n\n" + base`, concatenating the whole document
on every frame, and `linesFor` then compared that fresh string against
`cacheBase`. Without a banner the concatenation is skipped and the memoized
`joined` string short-circuits the compare, so the benchmark looked flat while
the real UI paid for the whole history each frame.

Measured per-frame refresh + `scrollView.View` with the banner present:

| blocks | before |    after |
| -----: | -----: | -------: |
|    100 |  47 µs | **2.2 µs** |
|    500 | 116 µs | **2.4 µs** |
|   2000 | 445 µs | **2.3 µs** |
|   5000 | 1.32 ms | **2.5 µs** |

- `stableBase` is split into `bodyBase` (chunks plus the unfolded tail) and the
  banner-leading composition. `linesFor` caches the banner and the body
  separately, so a frame that changes neither copies nothing and the change check
  compares the memoized body rather than a fresh concatenation.
- `bannerText` depends only on the mark and the width, so it is memoized; it runs
  on every frame and otherwise re-rendered the mark through lipgloss each time.
- `BenchmarkViewportRefresh` now sets the production banner, so the guarding
  benchmark measures the real frame.

## Verification

- Keep the `internal/ui` benchmarks and record before/after numbers here.
- The existing `internal/ui` suite must stay green; its `plain` helper
  normalizes ANSI so cached and uncached renders compare directly.
- Prefer output-equivalence tests (incremental vs. from-scratch) over
  timing-only assertions, since wall-clock timing is noisy in CI.
