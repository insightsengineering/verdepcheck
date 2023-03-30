withr::local_envvar(
  R_USER_CACHE_DIR = tempfile(),
  GITHUB_PAT = Sys.getenv("GITHUB_PAT", Sys.getenv("GITHUB_TOKEN", "")),
  .local_envir = teardown_env()
)
