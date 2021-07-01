# Packages

version       = "0.6.0"
author        = "Maurizio Tomasi <ziotom78 .at. gmail.com>"
description   = "Toml parser library for Nim"
license       = "MIT"
srcDir        = "src"
skipDirs      = @["decoder"]

# Deps

requires "nim >= 0.18.0"

from os import `/`, expandTilde
from strutils import `%`

task run_toml_test, "Validates parsetoml using toml-test":
  exec("nim c -d:release decoder/decoder.nim")
  # Needs "go" executable to be present in PATH.
  # In GHA, add "- uses: actions/setup-go@v2"
  let
    goPath = getEnv("GOPATH")
    tomlTestRepo = "github.com/BurntSushi/toml-test/cmd/toml-test@master"
  doAssert goPath != ""
  exec("go get -u -v " & tomlTestRepo)
  exec("$#/bin/toml-test" % [goPath] & " decoder/decoder")

task run_toml_test_with_skips, "Validates parsetoml using toml-test (with test skips)":
  exec("nim c -d:release decoder/decoder.nim")
  # Needs "go" executable to be present in PATH.
  # In GHA, add "- uses: actions/setup-go@v2"
  let
    goPath = getEnv("GOPATH")
    tomlTestRepo = "github.com/BurntSushi/toml-test/cmd/toml-test@master"
  doAssert goPath != ""
  exec("go get -u -v " & tomlTestRepo)
  exec("$#/bin/toml-test" % [goPath] &
    " -skip valid/array" &
    " -skip valid/array-bool" &
    " -skip valid/array-empty" &
    " -skip valid/array-hetergeneous" &
    " -skip valid/array-mixed-int-array" &
    " -skip valid/array-mixed-int-float" &
    " -skip valid/array-mixed-int-string" &
    " -skip valid/array-mixed-string-table" &
    " -skip valid/array-nested-double" &
    " -skip valid/array-nested" &
    " -skip valid/array-nospaces" &
    " -skip valid/array-string-quote-comma-2" &
    " -skip valid/array-string-quote-comma" &
    " -skip valid/array-string-with-comma" &
    " -skip valid/array-strings" &
    " -skip valid/comment-everywhere" &
    " -skip valid/comment-tricky" &
    " -skip valid/datetime-local-date" &
    " -skip valid/datetime-local-time" &
    " -skip valid/example" &
    " -skip valid/float-inf-and-nan" &
    " -skip valid/float-zero" &
    " -skip valid/inline-table-key-dotted" &
    " -skip valid/inline-table-nest" &
    " -skip valid/multiline-string-quotes" &
    " -skip valid/multiline-string" &
    " -skip valid/spec-example-1-compact" &
    " -skip valid/spec-example-1" &
    " -skip invalid/array-missing-separator" &
    " -skip invalid/array-of-tables-1" &
    " -skip invalid/control-comment-del" &
    " -skip invalid/control-comment-lf" &
    " -skip invalid/control-comment-null" &
    " -skip invalid/control-comment-us" &
    " -skip invalid/control-string-bs" &
    " -skip invalid/duplicate-table-array2" &
    " -skip invalid/encoding-bad-utf8-in-comment" &
    " -skip invalid/encoding-bad-utf8-in-string" &
    " -skip invalid/encoding-utf16" &
    " -skip invalid/inline-table-double-comma" &
    " -skip invalid/inline-table-no-comma" &
    " -skip invalid/inline-table-trailing-comma" &
    " -skip invalid/integer-double-sign-nex" &
    " -skip invalid/integer-double-sign-plus" &
    " -skip invalid/integer-leading-zero-sign-1" &
    " -skip invalid/integer-leading-zero-sign-2" &
    " -skip invalid/key-multiline" &
    " -skip invalid/string-bad-multiline" &
    " -skip invalid/string-multiline-escape-space" &
    " -skip invalid/string-multiline-escape-space" &
    " decoder/decoder")
