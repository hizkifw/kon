// The website is not Go. Its own module boundary keeps kon's ./... patterns
// out of website/node_modules, where npm packages can ship .go files, and
// keeps the site out of the module zip that go install downloads.
module github.com/hizkifw/kon/website

go 1.25.0
