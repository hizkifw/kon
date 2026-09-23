# Adding a storage migration

Use a migration when a release changes durable files that an existing kon
installation may contain. `internal/migrate` provides version tracking and
process locks. `internal/migrations` contains the conversion code and the
explicit [step order](../../internal/migrations/registry.go).

## Add the step

1. Find the last step in `migrations.Ordered()` and choose the next storage
   version. Storage versions are consecutive and independent of app releases
   and individual session format versions.
2. Add a descriptively named file under `internal/migrations`, prefixed with
   that version, such as `v003_move_sessions.go`. Implement `migrate.Step`:

   ```go
   type Step interface {
       Version() int // Storage version produced by this step.
       Name() string // Short progress message.
       Run(context.Context, config.Paths) error
   }
   ```

3. Append the step to `migrations.Ordered()`. Keep all existing entries in
   place. The runner checks for gaps and rejects an out-of-order registry.
4. Update the affected format documentation and add a fixture for the old
   layout when the conversion needs one.

`Run` receives paths to kon's config and data files. It runs before config,
history, or sessions are loaded, so it must read the old representation itself.
It may move files or transform them as needed. Check the context during long
walks and return a useful error that identifies the file that failed.

## Make retries safe

The runner records a version only after `Run` returns successfully. A crash or
error causes the same step to run again on the next launch. Design each file
operation to tolerate that replay:

- Recognize both the old and converted state. Skip files already converted.
- Write and sync a complete replacement before moving the original. Keep a
  recoverable source until the replacement is installed and validated.
- Use stable backup names so a retry can recognize an interrupted rename, and
  clean up abandoned temporary files.
- Preserve IDs, parent links, and the decoded root system prompt when changing
  session files. The prompt must remain byte-identical to retain its provider
  cache prefix.
- Never move or delete `storage-version`, `upgrade.gate.lock`, or
  `instances.lock` while a migration is running.

The version marker is an append-only file at `<data-dir>/storage-version` with
one completed version per line. Its small read is the normal startup check;
current installations do not scan the session tree. An incomplete final marker
line is ignored and repaired on retry. A newer marker is rejected by an older
binary.

## Test and verify

Test the conversion through `migrate.Enter` with temporary config and data
roots. Verify that the new reader opens the result and that a second launch
leaves it unchanged. Exercise failure before replacement and recovery from a
partially completed rename. If a step touches several files, include a mixed
old/new fixture to prove that a retry finishes the remaining work. Test that
bad input reports an error without advancing the version marker.

Run `make check` before finishing. Run `make test-race` when changing the lock
protocol or other concurrent behavior.

## Startup coordination

Every command that accesses durable state holds a shared `instances.lock` for
its lifetime. Startup briefly holds a shared `upgrade.gate.lock` while joining
the active instances. A migration takes the gate exclusively to stop new
instances, then waits for an exclusive instances lock before changing files.
This work completes before the TUI opens. After `kon upgrade` replaces the
executable, it runs the new binary as `kon upgrade --finalize`, which enters
storage and then refreshes the model catalog, so pending steps run during the
upgrade instead of on the next launch. The release installers (`scripts/install.sh`
and `scripts/install.ps1`) invoke the same subcommand after placing the binary,
so a fresh install migrates and refreshes before the first launch. The upgrading
process holds no lease while it waits, and
finalize takes no arguments: steps must derive everything from the files on
disk. Keep lock acquisition and version
tracking in `internal/migrate`; concrete steps belong in
`internal/migrations`.
