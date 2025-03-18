# todos ------------------------------------------------------------------------

# TODO: BEFORE you start implementing a bunch of stuff, now that you've got the
# bones you should CREATE A TESTING SUITE. Make a bunch of tests and supports
# now, so you know the basics are robust BEFORE you waste time implementing a
# billion other features.
#
# Most of what you need:
# - automatic message conversion
# - function registration
# - `stop_if_not()`
#
# Is already implemented. So it's really just about adding features now. Which is
# why you SHOULD TEST THE FRAMEWORK FIRST.

# expr_to_message --------------------------------------------------------------

# NOTE: It's important that we know the `env` message many of the messages will
# need to evaluate their individual components in the correct environment. For
# example, to generate a message for:
#
# `length(symbol) == literal`
#
# We likely get `expr_eval = FALSE`, but we also need to re-evaluate `length(symbol)`
# in the correct environment in order to put `symbol`'s length in the error message.
#
# `expr`      -> expression to process
# `expr_eval` -> the expression `expr` after evaluation
# `env`       -> environment to evaluate message components in
expr_to_message <- function(expr, expr_eval, env = rlang::caller_env()) {
  ## Level 1
  switch(
    expr_type(expr),

    ## Level 2
    call = switch(
      call_type(expr, env = env),

      ## Level 3

      # operator-`(`
      # Redundant braces, e.g. `(length(x) == y)`, means opt-out of custom message
      bracket = default_msg(expr[[2]], expr_eval),

      # operator-`==`
      equals = switch(
        equals_type(expr),
        # `length(symbol) == symbol` (commutative)
        length_equals_sym = length_equals_sym_msg(
          length_arg_x = expr[[2]][[2]],      # `symbol` of `length(symbol)`
          sym = expr[[3]],                    # `symbol` of `== symbol`
          length_eval = eval(expr[[2]], env), # `eval(length(symbol))`
          sym_eval = eval(expr[[3]], env)     # `eval(symbol)`
        ),
        sym_equals_length = length_equals_sym_msg(
          length_arg_x = expr[[3]][[2]],
          sym = expr[[2]],
          length_eval = eval(expr[[3]], env),
          sym_eval = eval(expr[[2]], env)
        ),
        # `length(symbol) == literal` (commutative)
        length_equals_lrl = length_equals_lrl_msg(
          length_arg_x = expr[[2]][[2]],
          lrl = expr[[3]],
          length_eval = eval(expr[[2]], env)
        ),
        lrl_equals_length = length_equals_lrl_msg(
          length_arg_x = expr[[3]][[2]],
          lrl = expr[[2]],
          length_eval = eval(expr[[3]], env)
        ),

        # Level 3 default
        default_msg(expr, expr_eval)
      ), # operator-`==` ends

      # operator-`<=`
      leq = switch(
        leq_type(expr),
        # `length(symbol) <= literal`
        length_leq_lrl = length_leq_lrl_msg(
          length_arg_x = expr[[2]][[2]],
          lrl = expr[[3]],
          length_eval = eval(expr[[2]], env)
        ),
        # `length(symbol) <= symbol`
        length_leq_sym = length_leq_sym_msg(
          length_arg_x = expr[[2]][[2]],
          sym = expr[[3]],
          length_eval = eval(expr[[2]], env),
          sym_eval = eval(expr[[3]], env)
        ),

        # Level 3 default
        default_msg(expr, expr_eval)
      ), # operator-`<=` ends

      # `stopper_fun(...)` (call to a registered stopper function)
      stopper = stopper_msg(
        expr = expr,
        env = env,
        type = "default"
      ), # `stopper()` ends

      # Level 2 default
      default_msg(expr, expr_eval)
    ), # call expression type ends

    # Level 1 default
    default_msg(expr, expr_eval)
  )
}

# messengers -------------------------------------------------------------------

## Conventions:

# Shorthands:
# - expr -> any expression
# - lrl  -> syntactic literal
# - sym  -> symbol
# - obj  -> syntactic literal or symbol

# Call Processing Function names / conventions:
#
# The function `length_leq_sym_msg` processes a call like `length(x) <= max_len`.
# The idea is to make the function name look like the call it's processing, so:
# - `length`   -> the LHS `length(x)`
# - `_equals_` -> the operator `==`
# - `sym`      -> the RHS symbol `max_len`
#
# The arguments of `length_leq_sym_msg()` are also meant to mimic the call.
# - `length_arg_x` -> the `x` argument of `length(x)`
# - `sym`          -> the RHS symbol `max_len`
# - `length_eval`  -> the result of evaluating expression `length(x)`
# - `sym_eval`     -> the result of evaluating symbol `max_len`


## default ---------------------------------------------------------------------

# TODO: We'll want a default `all()`, `any()`, and `none()` message as well

default_msg <- function(expr, expr_eval) {
  # NOTE: For now we'll use `rlang::as_label()`, but I'll want some more control
  #       in the final solution.
  expr_label <- rlang::as_label(expr)
  bullet <- cli::format_inline(
    "Expected {.code {expr_label}} to be {.code TRUE}, not {.obj_type_friendly {expr_eval}}."
  )
  c(x = cli_as_is(bullet))
}

default_all_msg <- function(expr, expr_eval, all_arg_eval) {

}

default_any_msg <- function(expr, expr_eval) {

}

default_none_msg <- function(expr, expr_eval, none_arg_eval) {

}

# stopper ----------------------------------------------------------------------

stopper_msg <- function(
    expr,
    env,
    type = c("default", "not", "any", "all", "none")
  ) {

  type <- rlang::arg_match0(type, c("default", "not", "any", "all", "none"))
  call <- expr[[1]]
  fun <- eval(call, env)

  # Messengers are stored by namespace and function name, so we need both the
  # function (to get it's namespace) and the call (to get it's name).
  messenger <- get_messenger(fun, fun_expr = call)
  message_fun <- switch(
    type,
    default = get_message_fun(messenger),
    not = get_not_message_fun(messenger),
    any = get_any_message_fun(messenger),
    all = get_all_message_fun(messenger),
    none = get_none_message_fun(messenger)
  )
  message_fun(expr, env = env)
}

## length ----------------------------------------------------------------------

# TODO: Split off the cases `length_equals_symbol` and `length_equals_literal`. The
# exiting `length_equals_obj_msg` works for the literal case, but in the symbol case
# we'll want to evaluate the symbol as well

length_equals_sym_msg <- function(length_arg_x, sym, length_eval, sym_eval) {
  x_label <- rlang::as_label(length_arg_x)
  sym_label <- rlang::as_label(sym)
  bullet <- cli::format_inline(
    "{.arg {x_label}} must be length {.code {sym_label}} ({sym_eval}), not {length_eval}."
  )
  c(x = cli_as_is(bullet))
}

length_equals_lrl_msg <- function(length_arg_x, lrl, length_eval) {
  x_label <- rlang::as_label(length_arg_x)
  bullet <- cli::format_inline(
    "{.arg {x_label}} must be length {lrl}, not {length_eval}."
  )
  c(x = cli_as_is(bullet))
}

length_leq_sym_msg <- function(length_arg_x, sym, length_eval, sym_eval) {
  x_label <- rlang::as_label(length_arg_x)
  sym_label <- rlang::as_label(sym)
  bullet <- cli::format_inline(
    "{.arg {x_label}} is length {length_eval}, ",
    "but must be at most length {.code {sym_label}} ({sym_eval})."
  )
  c(x = cli_as_is(bullet))
}

length_leq_lrl_msg <- function(length_arg_x, lrl, length_eval) {
  x_label <- rlang::as_label(length_arg_x)
  bullet <- cli::format_inline(
    "{.arg {x_label}} is length {length_eval}, ",
    "but must be at most length {lrl}."
  )
  c(x = cli_as_is(bullet))
}

# expressions ------------------------------------------------------------------

# From: https://adv-r.hadley.nz/expressions.html#ast-funs
expr_type <- function(x) {
  if (rlang::is_syntactic_literal(x)) {
    "literal"
  } else if (is.symbol(x)) {
    "symbol"
  } else if (is.call(x)) {
    "call"
  } else if (is.pairlist(x)) {
    "pairlist"
  } else {
    typeof(x)
  }
}

# What kind of expression is a call. Currently it's one of:
# - "equals" -> `expression == expression`
# - "other"  -> `any_other_expression`
call_type <- function(expr, env) {
  call <- expr[[1]]
  # Handle operators (e.g. `==`, `+`), control flow (e.g. `(`), and then simple
  # function calls (e.g. `all(x)`, `rlang::is_string(x)`).
  if (identical(call, quote(`==`))) {
    "equals"
  } else if (identical(call, quote(`<=`))) {
    "leq"
  } else if (identical(call, quote(`(`))) {
    "bracket"
  } else if (rlang::is_call_simple(expr)) {
    # Simple calls are operators (e.g. `1 + 1`) or calls by name (e.g. `foo()`,
    # `bar::foo()`) and not things like `(function {})()`, `foo$bar()`
    if (is_stopper(fun = eval(call, env), fun_expr = call)) {
      "stopper"
    } else {
      "other"
    }
  } else {
    "other"
  }
}

# What kind of expression is this call to `==`. Currently, it's either:
# - "length_equals_obj" -> `length(symbol) == symbol/literal`
# - "obj_equals_length" -> `symbol/literal == length(symbol)`
# - "other"             -> `any_other_expression`
equals_type <- function(expr) {

  side_type <- function(side) {
    if (rlang::is_call(side, "length") && rlang::is_symbol(side[[2]])) {
      "length"
    } else if (rlang::is_syntactic_literal(side)) {
      "literal"
    } else if (rlang::is_symbol(side)) {
      "symbol"
    } else {
      "other"
    }
  }

  lhs <- side_type(expr[[2]])
  rhs <- side_type(expr[[3]])
  switch(
    paste0(lhs, "_", rhs),
    length_literal = "length_equals_lrl",
    length_symbol = "length_equals_sym",
    literal_length = "lrl_equals_length",
    symbol_length = "sym_equals_length",
    "other"
  )
}

# What kind of expression is this call to `<=`
leq_type <- function(expr) {

  side_type <- function(side) {
    if (rlang::is_call(side, "length") && rlang::is_symbol(side[[2]])) {
      "length"
    } else if (rlang::is_syntactic_literal(side)) {
      "literal"
    } else if (rlang::is_symbol(side)) {
      "symbol"
    } else {
      "other"
    }
  }

  lhs <- side_type(expr[[2]])
  rhs <- side_type(expr[[3]])
  switch(
    paste0(lhs, "_", rhs),
    length_literal = "length_leq_lrl",
    length_symbol = "length_leq_sym",
    "other"
  )
}

# testing ----------------------------------------------------------------------

if (FALSE) {
  load_all()

  x <- 1:3
  max_len <- 2

  # Default
  expr_to_message(expr = quote(10 > 12), expr_eval = FALSE)
  expr_to_message(expr = quote(1 + 1), expr_eval = 2)

  # length(symbol) == symbol/literal
  expr_to_message(expr = quote(length(x) == 10), expr_eval = FALSE)
  expr_to_message(expr = quote(length(x) == max_len), expr_eval = FALSE)
  expr_to_message(expr = quote(max_len == length(x)), expr_eval = FALSE)
  expr_to_message(expr = quote(11 == length(x)), expr_eval = FALSE)

  # length(symbol) <= symbol/literal
  expr_to_message(expr = quote(length(x) <= 5), expr_eval = FALSE)
  expr_to_message(expr = quote(length(x) <= max_len), expr_eval = FALSE)

  # (expression)
  expr_to_message(expr = quote((length(x) <= max_len)), expr_eval = FALSE)

  # stopper
  is_numeric <- function(x) is.numeric(x)
  register_stopper(
    is_numeric,
    "{.arg {name(x)}} must be numeric, not {.obj_type_friendly {x}}."
  )
  y <- "A"
  cli::cli_inform(expr_to_message(expr = quote(is_numeric(y)), expr_eval = FALSE))

  # NOTE: `expr_to_message()`, using `switch()`, doesn't really take any time.
  # Almost all of the execution time is devoted to creating the message.
  env <- rlang::current_env()
  bench::mark(
    expr_to_message(expr = quote(is_numeric(y)), expr_eval = FALSE, env = env),
    stopper_msg(expr = quote(is_numeric(y)), env = env, type = "default"),
    iterations = 1000
  )
}
