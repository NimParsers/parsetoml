# Packages

version       = "0.3.2"
author        = "Maurizio Tomasi <ziotom78 .at. gmail.com>"
description   = "Toml parser library for Nim"
license       = "MIT"
srcDir        = "src"
skipDirs      = @["decoder"]

# Deps

requires "nim >= 0.15.0"

from ospaths import expandTilde

task run_toml_test, "Validates parsetoml using toml-test":
  exec("nim c -d:release decoder/decoder.nim")
  exec("go get github.com/BurntSushi/toml-test") # Set Travis "language:" to "go".
  let
    tomlTestBin = block:
                    var
                      temp = "~/go.apps/bin/toml-test".expandTilde
                    if not temp.fileExists():
                      temp = "~/go/bin/toml-test".expandTilde
                    temp
  exec(tomlTestBin & " decoder/decoder")
