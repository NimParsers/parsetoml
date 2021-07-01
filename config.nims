task pullConfig, "Fetch my global config.nims":
  exec("git submodule add -f -b master https://github.com/kaushalmodi/nim_config")
when fileExists("nim_config/config.nims"):
  include "nim_config/config.nims" # This gives "nim musl", "nim test" and "nim docs" that's run on CI
