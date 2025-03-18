# stop_if_not ------------------------------------------------------------------

# TODO: Add an `error_opts(...)` function for the other arguments that `cli::cli_abort()`
#       passes onto `rlang::abort()`. I'll leave the `cli_abort()` specific arguments
#       out here.
stop_if_not <- function(
    ...,
    .error_header = "Assertion failed:", # TODO: Make setting the default into an option!
    .error_envir = rlang::caller_env(),
    .error_call = .error_envir,
    .error_frame = .error_envir
) {

  n <- ...length()
  for (i in seq_len(n)) {
    result <- ...elt(i)

    # `stopifnot()` requires a result to be `TRUE` or a vector of `TRUE`
    if (!(is.logical(result) && !anyNA(result) && all(result))) {
      # Grab the i-th dot's expression and name from the arguments of this call
      args <- match.call()[-1L]
      expr <- args[[i]]
      message <- rlang::names2(args)[[i]]

      # An unnamed dot will receive a custom error message
      if (message == "") {
        message <- expr_to_message(expr, expr_eval = result, env = .error_envir)
      } else {
        # Because of how `stopifnot()` checks are named, there isn't a nice way
        # for a user to specify the bullet, but `x` is probably always alright.
        message <- c(x = message)
      }
      cli::cli_div(theme = stopr_theme())
      cli::cli_abort(
        c(.error_header, message),
        call = .error_call,
        .envir = .error_envir,
        .frame = .error_frame
      )
    }
  }
  invisible()
}

# stopifnot --------------------------------------------------------------------

# Deconstruction of `stopifnot()`, since we'll need to carefully re-implement it
# in `stopifnot2()`.
stopifnot_copy <- function (..., exprs, exprObject, local = TRUE) {
  n <- ...length()

  # This is the `exprs`, `exprObject` branch, which I don't care about
  if ((has.e <- !missing(exprs)) || !missing(exprObject)) {
    if (n || (has.e && !missing(exprObject)))
      stop("Only one of 'exprs', 'exprObject' or expressions, not more")
    envir <- if (isTRUE(local))
      parent.frame()
    else if (isFALSE(local))
      .GlobalEnv
    else if (is.environment(local))
      local
    else stop("'local' must be TRUE, FALSE or an environment")
    E1 <- if (has.e && is.call(exprs <- substitute(exprs)))
      exprs[[1]]
    cl <- if (is.symbol(E1) && E1 == quote(`{`)) {
      exprs[[1]] <- quote(stopifnot)
      exprs
    }
    else as.call(c(quote(stopifnot), if (!has.e) exprObject else as.expression(exprs)))
    names(cl) <- NULL
    return(eval(cl, envir = envir))
  }

  # This creates the correct label for an expression
  Dparse <- function(call, cutoff = 60L) {
    ch <- deparse(call, width.cutoff = cutoff)
    if (length(ch) > 1L)
      paste(ch[1L], "....")
    else ch
  }
  # Keeps the first `n` elements of `x`
  head <- function(x, n = 6L) x[seq_len(if (n < 0L) max(length(x) + n, 0L) else min(n, length(x)))]
  # Pastes together a multi-line message (?)
  abbrev <- function(ae, n = 3L) paste(c(head(ae, n), if (length(ae) > n) "...."), collapse = "\n  ")

  # Checks each `...`
  for (i in seq_len(n)) {
    r <- ...elt(i)
    if (!(is.logical(r) && !anyNA(r) && all(r))) {
      dots <- match.call()[-1L]
      if (is.null(msg <- names(dots)) || !nzchar(msg <- msg[i])) {
        cl.i <- dots[[i]]
        msg <- if (
          is.call(cl.i) && identical(cl.i[[1]], quote(all.equal)) && (is.null(ni <- names(cl.i)) ||
          length(cl.i) == 3L || length(cl.i <- cl.i[!nzchar(ni)]) == 3L)
        ) {
          sprintf(gettext("%s and %s are not equal:\n  %s"), Dparse(cl.i[[2]]), Dparse(cl.i[[3]]), abbrev(r))
        } else {
          sprintf(ngettext(length(r), "%s is not TRUE", "%s are not all TRUE"), Dparse(cl.i))
        }
        stop(simpleError(msg, call = if (p <- sys.parent(1L)) sys.call(p)))
      }
    }
  }

  invisible()
}

# testing ----------------------------------------------------------------------

if (FALSE) {
  load_all()

  x <- 1:5
  stop_if_not(length(x) == 2)
  stop_if_not(length(x) <= 2)
  stop_if_not(is.character(x))

  stop_if_not("`x` must be a character, not {.a {x}}." = is.character(x))
  stop_if_not(
    "`x` must have a maximum of 3 or less, but `x` is above 3 {.at {x >= 4}}." = max(x) < 4,
    .error_header = NULL
  )

  # Note, we are much slower which can't be avoided
  bench::mark(
    stopr = try(stop_if_not(length(x) <= 2)),
    base = try(stopifnot(length(x) <= 2)),
    check = FALSE
  )
  # Just `cli::cli_abort()` has a lot of overhead compared to stop, but only
  # when using string interpolation
  bench::mark(
    cli_with_interpolation = try(cli::cli_abort("{x}")),
    cli_fmt_inline = try(cli::format_inline("{x}")),
    rlang_abort = try(rlang::abort("")),
    base_stop = try(stop()),
    check = FALSE
  ) |> dplyr::select(1:5)
}
