# kon

How to build:

```sh
# Install stringer
go install golang.org/x/tools/cmd/stringer@latest

# Generate enums
go generate ./...

# Build
go build

# Run
go run . examples/hello.kon
```
