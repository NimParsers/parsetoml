# Packages

version       = "0.4.0"
author        = "Maurizio Tomasi <ziotom78 .at. gmail.com>"
description   = "Toml parser library for Nim"
license       = "MIT"
srcDir        = "src"
skipDirs      = @["decoder"]

# Deps

requires "nim >= 0.18.0"

from ospaths import `/`, expandTilde

task run_toml_test, "Validates parsetoml using toml-test":
  exec("nim c -d:release decoder/decoder.nim")
  # Needs "go" executable to be present in PATH.
  # For Travis, set "language:" to "go".
  let
    goPath = getEnv("GOPATH")
    tomlTestRepo = "github.com/BurntSushi/toml-test"
  doAssert goPath != ""
  exec("go get -u -v " & tomlTestRepo)
  exec((goPath / "bin" / "toml-test") & " " & "decoder/decoder")
