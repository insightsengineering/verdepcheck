`%nin%` <- Negate(`%in%`)

.desc_field <- "Config/Needs/verdepcheck"

default_config <- function() {
  list(
    dependencies = .desc_field,
    library = tempfile()
  )
}
append_config <- function(x1, x2) {
  append(x1, x2)[unique(c(names(x1), names(x2)))]
}
