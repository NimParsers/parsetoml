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

task run_toml_test, "Validates parsetoml using toml-test":
  exec("nim c -d:release decoder/decoder.nim")
  # Needs "go" executable to be present in PATH.
  # In GHA, add "- uses: actions/setup-go@v2"
  let
    goPath = getEnv("GOPATH")
    # tomlTestRepo = "github.com/BurntSushi/toml-test@master" # This does not install the toml-test binary
    # https://github.com/BurntSushi/toml-test/commit/9767d201b51ac9c50630f181828bcd922bf3e9e5
    tomlTestRepo = "github.com/BurntSushi/toml-test@9767d201"
  doAssert goPath != ""
  exec("go get -u -v " & tomlTestRepo)
  exec((goPath / "bin" / "toml-test") & " " & "decoder/decoder")

task run_new_toml_test, "Validates parsetoml using toml-test from sgarciac":
  exec("nim c -d:release -d:newtestsuite decoder/decoder.nim")
  # Needs "go" executable to be present in PATH.
  # In GHA, add "- uses: actions/setup-go@v2"
  let
    goPath = getEnv("GOPATH")
    tomlTestRepo = "github.com/sgarciac/toml-test"
  doAssert goPath != ""
  exec("go get -u -v " & tomlTestRepo)
  exec("cp -r " & (goPath / "src" / "github.com" / "sgarciac" / "toml-test") &
    " " & (goPath / "src" / "github.com" / "BurntSushi" / "toml-test"))
  exec((goPath / "bin" / "toml-test") & " " & "decoder/decoder")
