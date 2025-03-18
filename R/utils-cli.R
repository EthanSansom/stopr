# stopr theme ----------------------------------------------------------------

stopr_theme <- function() {
  cli_theme <- cli::builtin_theme() # Required to alias `.obj_type_friendly`
  spans <- list(
    span.as_is = list(),
    span.a = cli_theme$span.obj_type_friendly,
    span.at = list(transform = at_locations),
    span.at_locations = list(transform = at_locations)
  )
  c(stopr_bullets(), spans)
}

stopr_bullets <- function() {
  bullet_symbols <- list(
    `x` = cli::col_red(cli::symbol$cross),
    `!` = cli::col_yellow("!"),
    `*` = cli::col_cyan(cli::symbol$bullet),
    `i` = cli::col_cyan(cli::symbol$info),
    `v` = cli::col_green(cli::symbol$tick),
    `>` = cli::symbol$arrow_right
  )
  exdents <- list(single = 2, double = 4, triple = 6)
  spaces <- list(single = "", double = "  ", triple = "    ")

  list(
    # Double indent
    `.bullets .bullet-xx` = list(
      `text-exdent` = exdents$double,
      before = function(x) paste0(spaces$double, bullet_symbols$`x`, " ")
    ),
    `.bullets .bullet-!!` = list(
      `text-exdent` = exdents$double,
      before = function(x) paste0(spaces$double, bullet_symbols$`!`, " ")
    ),
    `.bullets .bullet-**` = list(
      `text-exdent` = exdents$double,
      before = function(x) paste0(spaces$double, bullet_symbols$`*`, " ")
    ),
    `.bullets .bullet-ii` = list(
      `text-exdent` = exdents$double,
      before = function(x) paste0(spaces$double, bullet_symbols$`i`, " ")
    ),
    `.bullets .bullet-vv` = list(
      `text-exdent` = exdents$double,
      before = function(x) paste0(spaces$double, bullet_symbols$`v`, " ")
    ),
    `.bullets .bullet->>` = list(
      `text-exdent` = exdents$double,
      before = function(x) paste0(spaces$double, bullet_symbols$`>`, " ")
    ),
    # Triple indent
    `.bullets .bullet-xxx` = list(
      `text-exdent` = exdents$triple,
      before = function(x) paste0(spaces$triple, bullet_symbols$`x`, " ")
    ),
    `.bullets .bullet-!!!` = list(
      `text-exdent` = exdents$triple,
      before = function(x) paste0(spaces$triple, bullet_symbols$`!`, " ")
    ),
    `.bullets .bullet-***` = list(
      `text-exdent` = exdents$triple,
      before = function(x) paste0(spaces$triple, bullet_symbols$`*`, " ")
    ),
    `.bullets .bullet-iii` = list(
      `text-exdent` = exdents$triple,
      before = function(x) paste0(spaces$triple, bullet_symbols$`i`, " ")
    ),
    `.bullets .bullet-vvv` = list(
      `text-exdent` = exdents$triple,
      before = function(x) paste0(spaces$triple, bullet_symbols$`v`, " ")
    ),
    `.bullets .bullet->>>` = list(
      `text-exdent` = exdents$triple,
      before = function(x) paste0(spaces$triple, bullet_symbols$`>`, " ")
    )
  )
}

# helpers ----------------------------------------------------------------------

at_locations <- function(x) {
  # TODO: Better error message
  stopifnot(rlang::is_integerish(x) || is.logical(x))

  locations <- if (is.logical(x)) which(x & !is.na(x)) else x
  locations <- as.numeric(locations) # Deparse integer as `1` not `1L`
  n <- length(locations)
  if (n == 0) {
    return("at no locations")
  }
  n_max <- 5
  at <- ngettext(min(n, n_max), "at location ", "at locations ")
  if (n > n_max) {
    paste0(at, "`", deparse(locations[seq(n_max)]), "` and ", n - n_max, " more")
  } else {
    paste0(at, "`", deparse(locations), "`")
  }
}

# `.as_is` is a span in the `stopr_theme()`, which indicates that we don't want
# to touch the text inside. This allows us to safely pass `cli::cli_abort()` a
# pre-formatted message (i.e. using `cli::format_inline()`).
cli_as_is <- function(x) {
  paste0("{.as_is ", x, "}")
}

# testing ----------------------------------------------------------------------

if (FALSE) {
  load_all()

  # Indented bullets
  cli::cli_div(theme = stopr_theme())
  cli::cli_inform(
    c(
      i = "One indent info",
      ii = "Two indent info",
      ii = paste(rep("Two indent info wrapped. ", 10), collapse = " ")
    )
  )
  cli::cli_end()

  # Spans .a, .at/.at_locations, .as_is
  cli::cli_div(theme = stopr_theme())
  cli::cli_inform(
    c(
      "{.a {10}} and {.obj_type_friendly {10}}",
      "{.at {c(TRUE, FALSE, TRUE)}} and {.at_locations {1:20}}",
      "{.as_is hello this is some text} and {.as_is {10}}"
    )
  )
  cli::cli_end()

  # Weirdly enough, the indented bullets are a little faster since we pre-color
  # the bullet (e.g. `cli::col_yellow("!")`) in `stopr_theme()`, so we call the
  # color function only once.
  cli::cli_div(theme = stopr_theme())
  bench::mark(
    cli::cli_inform(c(x = 0, v = 1, `!` = "!")),
    cli::cli_inform(c(xx = 0, vv = 1, `!!` = "!"))
  )
  cli::cli_end()
}
