package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"

	"github.com/hizkifw/kon/internal/buildinfo"
	"github.com/hizkifw/kon/internal/catalog"
	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/selfupdate"
)

func upgradeCommand() command {
	return command{
		name:     "upgrade",
		summary:  "install the latest kon release",
		synopsis: "kon upgrade [--check]",
		detail: "Download the latest release from GitHub, verify its checksum, and replace\n" +
			"this executable. The new binary then applies any pending storage migrations\n" +
			"and refreshes the model catalog.\n\n" +
			"  --check      report whether a newer release exists without installing it\n" +
			"  --finalize   apply pending storage migrations and refresh the model catalog;\n" +
			"               the new binary runs this itself after an upgrade, and it is\n" +
			"               safe to repeat",
		run: runUpgradeCmd,
	}
}

func runUpgradeCmd(args []string) error {
	fs := flag.NewFlagSet("kon upgrade", flag.ContinueOnError)
	fs.SetOutput(io.Discard)
	fs.Usage = func() {}
	check := fs.Bool("check", false, "report without installing")
	finalize := fs.Bool("finalize", false, "apply pending storage migrations and refresh the model catalog")
	if err := fs.Parse(args); err != nil {
		return err
	}
	if fs.NArg() > 0 || (*check && *finalize) {
		return fmt.Errorf("usage: kon upgrade [--check]")
	}
	if *finalize {
		return runFinalize()
	}
	return runUpgrade(*check)
}

// runFinalize is the entrypoint every earlier kon invokes on its replacement,
// so it must keep accepting "upgrade --finalize" with no other arguments. It
// takes no input from the old binary: the storage marker on disk and this
// binary's own migration registry decide what runs.
func runFinalize() error {
	paths, err := config.ResolvePaths()
	if err != nil {
		return err
	}
	return withStorage(paths, func() error {
		// By now the new binary is installed and storage is migrated, so a
		// failed refresh only warns: the bundled snapshot remains usable and
		// "kon models --refresh" can retry it.
		fmt.Fprintln(os.Stderr, "refreshing model catalog")
		if err := refreshCatalog(context.Background(), paths.Catalog); err != nil {
			fmt.Fprintf(os.Stderr, "kon: warning: refresh model catalog: %v\n", err)
		}
		return nil
	})
}

// refreshCatalog is a variable so tests can finalize without contacting
// models.dev.
var refreshCatalog = func(ctx context.Context, cachePath string) error {
	service, err := catalog.New(cachePath)
	if err != nil {
		return err
	}
	return service.Refresh(ctx)
}

func runUpgrade(checkOnly bool) error {
	current, err := selfupdate.ParseVersion(buildinfo.Version())
	if err != nil {
		return fmt.Errorf("cannot upgrade a development build (version %q); "+
			"reinstall with go install or the release installer", buildinfo.Version())
	}
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt)
	defer stop()
	updater := selfupdate.New()
	latest, err := updater.Latest(ctx)
	if err != nil {
		return err
	}
	if latest.Compare(current) <= 0 {
		fmt.Printf("kon %s is up to date (latest release is %s)\n", current, latest)
		return nil
	}
	if checkOnly {
		fmt.Printf("kon %s is available (current %s); run \"kon upgrade\" to install it\n", latest, current)
		return nil
	}

	// Resolve symlinks so an installer's symlink keeps pointing at the
	// upgraded file instead of being replaced by a regular file.
	exe, err := os.Executable()
	if err != nil {
		return fmt.Errorf("locate kon executable: %w", err)
	}
	if resolved, err := filepath.EvalSymlinks(exe); err == nil {
		exe = resolved
	}
	fmt.Fprintf(os.Stderr, "downloading kon %s\n", latest)
	if err := updater.Install(ctx, latest, exe); err != nil {
		return fmt.Errorf("upgrade to %s: %w", latest, err)
	}

	// This process holds no storage lease, so the new binary can take the
	// exclusive locks its migrations need. The child is not tied to ctx:
	// Ctrl+C reaches it directly, and it stops between retry-safe steps
	// rather than being killed mid-write.
	finalize := exec.Command(exe, "upgrade", "--finalize")
	finalize.Stdin, finalize.Stdout, finalize.Stderr = os.Stdin, os.Stdout, os.Stderr
	if err := finalize.Run(); err != nil {
		return errors.Join(
			fmt.Errorf("kon %s is installed, but finishing the upgrade failed; the next launch retries pending migrations", latest),
			err,
		)
	}
	fmt.Printf("upgraded kon %s → %s\n", current, latest)
	return nil
}
