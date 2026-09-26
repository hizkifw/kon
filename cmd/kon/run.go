package main

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/signal"
	"strings"
	"syscall"

	"github.com/hizkifw/kon/internal/app"
	"github.com/hizkifw/kon/internal/buildinfo"
	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/headless"
	"github.com/hizkifw/kon/internal/typedid"
)

const runSynopsis = "kon run [flags] [message...]"

func runCommand() command {
	return command{
		name:     "run",
		summary:  "send one prompt without the full-screen UI",
		synopsis: runSynopsis,
		detail: "Send one prompt in the current directory and stream the reply to stdout.\n" +
			"The message is the arguments after the flags, or piped stdin when there\n" +
			"are none. With --stdin, stdin is appended to the arguments after a blank line.\n" +
			"Tools run without confirmation, as they do in the full-screen UI.\n\n" +
			"  --model <name>         use this model for this run; the config is not changed\n" +
			"  --effort <level>       use this reasoning effort for this run\n" +
			"  --resume, -r           continue the most recent session in this directory\n" +
			"  --resume=<id>          continue a specific session\n" +
			"  --format text|json     stream text (default), or write one JSON event per line\n" +
			"  --stdin                append stdin to the message\n\n" +
			"Exit status is 0 when the turn completes, 1 on error, 2 on a usage error,\n" +
			"and 130 when interrupted.",
		run: runRun,
	}
}

// runArgs is the parsed command line of kon run.
type runArgs struct {
	message       []string
	model, effort string
	resume        bool
	resumeID      typedid.SessionID
	format        headless.Format
	// stdin appends stdin to a message given as arguments. Without it stdin
	// is read only when there is no message, so a caller that leaves stdin
	// open without writing to it cannot stall a run it gave a message.
	stdin bool
}

// parseRunArgs reads flags up to the first word of the message, or "--", so a
// message may contain words that look like flags. It is hand-written for the
// same reason as the root command's: --resume takes an optional value.
func parseRunArgs(args []string) (runArgs, error) {
	parsed := runArgs{format: headless.FormatText}
	value := func(i *int, name string) (string, error) {
		if *i+1 >= len(args) {
			return "", fmt.Errorf("%s needs a value", name)
		}
		*i++
		return args[*i], nil
	}
	for i := 0; i < len(args); i++ {
		arg := args[i]
		name, inline, hasInline := strings.Cut(arg, "=")
		var err error
		switch {
		case arg == "--":
			parsed.message = args[i+1:]
			return parsed, parsed.check()
		case !strings.HasPrefix(arg, "-"):
			parsed.message = args[i:]
			return parsed, parsed.check()
		case arg == "--resume" || arg == "-r":
			parsed.resume = true
			if i+1 < len(args) && strings.HasPrefix(args[i+1], "ses_") {
				i++
				parsed.resumeID, err = typedid.ParseSessionID(args[i])
			}
		case arg == "--stdin":
			parsed.stdin = true
		case name == "--resume" && hasInline:
			parsed.resume = true
			parsed.resumeID, err = typedid.ParseSessionID(inline)
		case name == "--model" || name == "--effort" || name == "--format":
			v := inline
			if !hasInline {
				v, err = value(&i, name)
			}
			switch name {
			case "--model":
				parsed.model = v
			case "--effort":
				parsed.effort = v
			default:
				parsed.format = headless.Format(v)
			}
		default:
			return runArgs{}, fmt.Errorf("unknown flag %q", arg)
		}
		if err != nil {
			return runArgs{}, err
		}
	}
	return parsed, parsed.check()
}

func (a runArgs) check() error {
	if a.format != headless.FormatText && a.format != headless.FormatJSON {
		return fmt.Errorf("--format must be text or json, not %q", a.format)
	}
	return nil
}

// prompt joins the message words. Stdin is the message when there are no
// words and it is piped, and is appended after them with --stdin, so
// `git diff | kon run --stdin review this` sends the instruction and then the
// diff.
func (a runArgs) prompt(stdin io.Reader, piped bool) (string, error) {
	prompt := strings.Join(a.message, " ")
	if a.stdin || (len(a.message) == 0 && piped) {
		b, err := io.ReadAll(stdin)
		if err != nil {
			return "", fmt.Errorf("read stdin: %w", err)
		}
		if input := strings.TrimRight(string(b), "\n"); strings.TrimSpace(input) != "" {
			if prompt != "" {
				prompt += "\n\n"
			}
			prompt += input
		}
	}
	if strings.TrimSpace(prompt) == "" {
		return "", errors.New("no message: pass it as arguments or on stdin")
	}
	return prompt, nil
}

func runRun(args []string) error {
	parsed, err := parseRunArgs(args)
	if err != nil {
		return usageError(err)
	}
	prompt, err := parsed.prompt(os.Stdin, !isTerminal(os.Stdin))
	if err != nil {
		return usageError(err)
	}
	paths, err := config.ResolvePaths()
	if err != nil {
		return err
	}
	return withStorage(paths, func() error {
		cfg, err := config.Initialize(paths)
		if err != nil {
			return err
		}
		cwd, err := workingDirectory()
		if err != nil {
			return err
		}
		runtime, err := app.Start(cfg, paths, cwd, buildinfo.Version(), app.Options{
			Resume: parsed.resume, SessionID: parsed.resumeID, Model: parsed.model, Effort: parsed.effort,
		})
		if err != nil {
			return err
		}
		out := headless.Output{Format: parsed.format, Stdout: os.Stdout, CWD: cwd}
		// Progress is for a person watching; a log or a pipe gets only the
		// conversation.
		if parsed.format == headless.FormatText && isTerminal(os.Stderr) {
			out.Progress = os.Stderr
		}
		ctx, stop := interruptible(runtime)
		defer stop()
		runErr := headless.Run(ctx, runtime, prompt, out)
		closeErr := runtime.Close()
		if ctx.Err() != nil {
			return errors.Join(&exitError{code: 130}, closeErr)
		}
		return errors.Join(runErr, closeErr)
	})
}

// interruptible cancels the run on the first interrupt or terminate signal,
// as the first Esc does in the full-screen UI, which stops a running command
// cleanly. A later interrupt escalates to killing a command that ignored it.
func interruptible(runtime *app.Runtime) (context.Context, func()) {
	ctx, cancel := context.WithCancel(context.Background())
	signals := make(chan os.Signal, 1)
	signal.Notify(signals, os.Interrupt, syscall.SIGTERM)
	done := make(chan struct{})
	go func() {
		presses := 0
		for {
			select {
			case <-signals:
				presses++
				cancel()
				if presses > 1 {
					runtime.Interrupt(presses)
				}
			case <-done:
				return
			}
		}
	}()
	return ctx, func() {
		signal.Stop(signals)
		close(done)
		cancel()
	}
}

func isTerminal(f *os.File) bool {
	info, err := f.Stat()
	return err == nil && info.Mode()&os.ModeCharDevice != 0
}
