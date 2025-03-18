
# `.as_is` is a custom {cli} span for the `stopr_theme()`, which indicates that
# we don't want to touch the text inside. This allows us to safely pass `cli::cli_abort()`
# a pre-formatted message (i.e. using `cli::format_inline()`).
cli_as_is <- function(x) {
  paste0("{.as_is ", x, "}")
}
