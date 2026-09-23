.DEFAULT_GOAL := help

.PHONY: help deps run fmt fmt-check vet build test test-race check smoke release clean catalog-update

VERSION ?= dev

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
	  '  make catalog-update  refresh the bundled models.dev snapshot' \
	  '  make release VERSION=v0.1.0'

deps:
	go mod download

run:
	go run ./cmd/kon

fmt:
	gofmt -w cmd internal

fmt-check:
	test -z "$$(gofmt -l cmd internal)"

vet:
	go vet ./...

build:
	mkdir -p bin
	CGO_ENABLED=0 go build -trimpath -ldflags="-X main.version=$(VERSION)" -o bin/kon ./cmd/kon

test:
	go test -shuffle=on ./...

test-race:
	go test -race -shuffle=on ./...

check: fmt-check vet test

smoke: build
	bin/kon --version

catalog-update:
	go generate ./internal/catalog

release:
	@test "$(VERSION)" != "dev" || (echo "VERSION is required, for example VERSION=v0.1.0"; exit 1)
	./scripts/release.sh "$(VERSION)"

clean:
	rm -rf bin dist
