.DEFAULT_GOAL := help

.PHONY: help deps run fmt fmt-check vet build test test-race check smoke bench loadtest release tag commit clean catalog-update

VERSION ?= dev
BUMP ?= patch

help:
	@printf '%s\n' \
	  'kon developer commands:' \
	  '  make deps       download authenticated Go modules' \
	  '  make run        run kon from source' \
	  '  make fmt        format Go source' \
	  '  make check      formatting, vet, and unit tests' \
	  '  make test-race  run the race detector' \
	  '  make build      build bin/kon' \
	  '  make smoke      build and check CLI startup' \
	  '  make bench      run the Go benchmarks' \
	  '  make loadtest   measure bin/kon CPU and memory under load (TUI=1 adds TUI scenarios)' \
	  '  make catalog-update  refresh the bundled models.dev snapshot' \
	  '  make release VERSION=v0.1.0' \
	  '  make tag [BUMP=patch|minor|major] [MODEL=name]  have kon tag the next release' \
	  '  make commit [MODEL=name]  have kon review, check, and commit the changes'

deps:
	go mod download

run:
	go run ./cmd/kon

fmt:
	gofmt -w cmd internal docs/product scripts

fmt-check:
	test -z "$$(gofmt -l cmd internal docs/product scripts)"

vet:
	go vet ./...

build:
	mkdir -p bin
	CGO_ENABLED=0 go build -trimpath -ldflags="-X github.com/hizkifw/kon/internal/buildinfo.version=$(VERSION)" -o bin/kon ./cmd/kon

test:
	go test -shuffle=on ./...

test-race:
	go test -race -shuffle=on ./...

check: fmt-check vet test

smoke: build
	bin/kon --version

bench:
	go test -run '^$$' -bench . -benchmem ./...

loadtest: build
	go run ./scripts/loadtest $(if $(TUI),-tui)

catalog-update:
	go generate ./internal/catalog

release:
	@test "$(VERSION)" != "dev" || (echo "VERSION is required, for example VERSION=v0.1.0"; exit 1)
	./scripts/release.sh "$(VERSION)"

# kon itself tags the release, following the Releases section of AGENTS.md.
# kon run appends piped stdin to its prompt, so both targets close stdin
# rather than wait on whatever make inherited.
tag:
	go run ./cmd/kon run $(if $(MODEL),--model $(MODEL)) \
	  "Tag the next $(BUMP) release of kon, following the Releases section of AGENTS.md. Do not push the tag." </dev/null

# kon reviews the changes, runs the checks, and commits them.
# The guard and the run share one shell, so a clean tree really stops here.
commit:
	@if [ -z "$$(git status --porcelain)" ]; then echo "nothing to commit"; exit 0; fi; \
	go run ./cmd/kon run $(if $(MODEL),--model $(MODEL)) \
	  "Review the uncommitted changes, run make check, and if it passes commit the changes. Write the commit message in the style of recent commits. Do not push." </dev/null

clean:
	rm -rf bin dist
