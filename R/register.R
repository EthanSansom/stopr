# register ---------------------------------------------------------------------
stoppers <- rlang::new_environment()

register_stopper <- function(fun, message) {
  fun_expr <- substitute(fun)
  stopifnot(
    "`fun` must be a function." = is.function(fun),
    "`fun` must be a function provided by name." = is.symbol(fun_expr) || rlang::is_call(fun_expr, c("::", ":::"))
  )
  stopifnot(
    "`message` must be a function with argument `call` or a non-NA non-empty character." =
    is.character(message) && length(message) >= 1 && !anyNA(message) ||
    is.function(message) && setequal(rlang::fn_fmls_names(message), "call")
  )
  stopifnot(
    "If `fun` is a primitive function, then `message` must be a function, not a character." =
    !is.primitive(fun) || is.function(message)
  )

  key <- fun_to_key(fun, fun_expr = fun_expr)
  if (is.character(message)) {
    message <- new_message_fun(fun, message)
  }
  assign(
    x = key,
    value = message,
    envir = stoppers
  )
}

# helpers ----------------------------------------------------------------------

# Convert a function `fun` into a key. If `fun` is associated with a namespace
# `ns`, it's key is `ns::fun`. Otherwise, it's key is `fun`.
# - fun_to_key(mean)       -> "base::mean"
# - fun_to_key(base::mean) -> "base::mean"
# - fun_to_key(foo)        -> "foo"
fun_to_key <- function(fun, fun_expr = substitute(fun)) {
  stopifnot(is.function(fun))
  ns <- environment(fun)
  if (rlang::is_call(fun_expr)) fun_expr <- fun_expr[[3]]
  fun_name <- rlang::as_string(fun_expr)

  if (rlang::is_namespace(ns)) {
    paste0(rlang::ns_env_name(ns), "::", fun_name)
  } else {
    fun_name
  }
}

# Creates a function to emit a message when `fun` is used as a stopper
new_message_fun <- function(fun, message) {
  force(fun)
  force(message)
  function(call) {
    # This is the environment that `message` is evaluated in during {cli}
    # string interpolation in `cli::format_inline()`.
    cli_env <- rlang::env_clone(rlang::current_env())

    # `args` are the unevaluated arguments to `call` (i.e. expressions) and
    # `args_eval` are the evaluated arguments to `call`.
    call_data <- call_data(call, fun, env = cli_env)
    args <- call_data$call_args
    args_eval <- call_data$call_args_eval

    # Provide accessors to the argument names (ala `rlang::caller_arg(x)`) used
    # in `call`. Note that `name(arg)` and `dot_name(arg)` will return NULL
    # whenever `arg` does not exist.
    args_names <- modify_tree(args, expr_label)
    dots_names <- args_names[["..."]]

    name <- function(sym) {
      index <- rlang::as_string(substitute(sym))
      args_names[[index]]
    }
    dot_name <- function(sym) {
      sym <- substitute(sym)
      if (rlang::is_scalar_integerish(sym)) {
        index <- as.integer(sym)
      } else {
        index <- rlang::as_string(substitute(sym))
      }
      dots_names[[index]]
    }

    # Provide accessors to all dots (as a list) and a given dot
    dots <- function() {
      cli_env[["..."]]
    }
    dot <- function(sym) {
      sym <- substitute(sym)
      if (rlang::is_scalar_integerish(sym)) {
        index <- as.integer(sym)
      } else {
        index <- rlang::as_string(substitute(sym))
      }
      cli_env[["..."]][[index]]
    }

    # Bind the evaluated arguments and helpers to `cli_env` so `cli::format_inline()`
    # can see them during string interpolation. Note that our helper functions
    # over-write any same-named arguments to `call`.
    rlang::env_bind(cli_env, !!!args_eval)
    rlang::env_bind(
      cli_env,
      name = name,
      dot_name = dot_name,
      dots = dots,
      dot = dot
    )

    formatted <- map_chr(message, cli::format_inline, .envir = cli_env)
    bullets <- rlang::names2(message)
    bullets[bullets == ""] <- "x"
    rlang::set_names(formatted, bullets)
  }
}

# Note, `defaults = TRUE, dots_expand = FALSE` won't do anything if the call
# is a primitive function. When registering a primitive function, the `message`
# argument in `register_stoppers` will need to be a manually created function.
call_match_args <- function(call, fun) {
  matched <- rlang::call_match(
    call = call,
    fn = fun,
    defaults = TRUE,
    dots_expand = FALSE
  )
  as.list(matched[-1L])
}

call_data <- function(call, fun, env) {
  ## Standardize the arguments of `call`
  call_args <- rlang::call_args(rlang::call_match(
    call = call,
    fn = fun,
    defaults = TRUE,
    dots_expand = FALSE
  ))

  ## Evaluate the arguments of `call`
  #
  # NOTE: This is actually a little tricky (I think) because arguments can
  # reference one-another (e.g. `foo = function(y = x, x = 10) {}`) and it's
  # not obvious in which order they should be evaluated. I've opted to create
  # and evaluate a new function which takes the same arguments as `call/fun` and
  # just returns those arguments in a list. This created function does whatever
  # magic R uses for promise evaluation by default, so I don't need to worry
  # about it.

  # Create a list of the form `list(arg1 = arg1, arg2 = arg2, ... = list2(...))`
  # which is used as the body of argument capturing function.
  fmls_names <- names(formals(fun))
  fmls_syms <- rlang::syms(fmls_names)
  fmls_syms[fmls_names == "..."] <- list(quote(rlang::list2(...)))
  args_list <- rlang::set_names(fmls_syms, fmls_names)

  # Create a new function with the same formals as the `call`'s function `fn`,
  # but which just returns it's evaluated arguments in a list.
  capture_args <- rlang::new_function(
    args = formals(fun),
    body = rlang::expr(list(!!!args_list)),
    env = env
  )
  # Evaluate the capturing function with the same arguments as the `call`
  call_args_eval <- eval(rlang::call2(capture_args, !!!rlang::call_args(call)), env)

  ## Return the pair
  list(
    call_args = call_args,
    call_args_eval = call_args_eval
  )
}

# TODO: For now I'm using `rlang::expr_text`, but maybe we want a custom solution?
#
# Convert an expression to a label for messages
expr_label <- function(expr) {
  rlang::expr_text(expr)
}

# interactive testing ----------------------------------------------------------

if (FALSE) {

  load_all()

  x_1 <- 10
  is_logical <- function(x) is.logical(x)
  is_logical_msg <- new_message_fun(
    is_logical,
    "{.arg {name(x)}} must be a logical vector, not {.obj_type_friendly {x}} ({x})."
  )
  is_logical_msg(quote(is_logical(x = x_1)))

  foo <- function(...) NULL
  foo_msg <- new_message_fun(
    foo,
    c(
      "{.arg {dot_name(a)}} is {dot(a)}, and `...` is {dots()}.",
      "{length(dots())} and {dots()[[2]]}."
    )
  )

  foo_msg(quote(foo(10, a = c(1, 2, 3))))

  bar <- function(y = x, x = 10) {
    NULL
  }
  bar_msg <- new_message_fun(
    bar,
    "{.arg {name(y)}} is {y}."
  )
  bar_msg(quote(bar()))

  my_env <- rlang::new_environment()
  assign("...", 10, my_env)

  call_args_1 <- function(x, ..., y = x, z = y, a = z) {
    env <- rlang::env_clone(rlang::current_env())
    args <- as.list(rlang::call_match(defaults = TRUE, dots_expand = FALSE)[-1L])

    rlang::env_bind(env, !!!args)
    lapply(args, eval, env)
  }
  call_dots <- function(x, ..., y = x, z = y, a = z) {
    list(x = x, `...` = list(...), y = y, z = z, a = a)
  }

  call_args_1(12, 10)
  call_dots(12, 10)

  call_args_2 <- function(x, ..., y = x, z = y, a = z) {
    args <- as.list(rlang::call_match(defaults = TRUE, dots_expand = FALSE)[-1L])

    args <-

    arg_names <- names(args)
    arg_syms <- rlang::syms(arg_names)
    arg_syms[arg_names == "..."] <- rlang::expr(list(...))

    body <- rlang::expr(!!rlang::set_names(arg_syms, arg_names))

    fun <- rlang::new_function(
      formals()
    )
  }

  # Lazy evaluation
  my_env <- rlang::env_clone(rlang::current_env())
  args <- call_args(x = 12)
  rlang::env_bind(my_env, !!!args)

  lapply(args, eval, my_env)

  call_args_1(x = as.Date("2020-01-01"), 12)
  call_dots(x = as.Date("2020-01-01"))

  eval(quote(y), my_env)
  eval(eval(quote(y), my_env), my_env)

  for (arg in names(args)) {
    my_env[[arg]] <- eval(rlang::sym(arg), my_env)
  }



  my_env[["x"]] |> eval(my_env)
  my_env[["y"]] |> eval(my_env)

}

# Testing how to evaluate the arguments of a call
if (FALSE) {
  eval_call_args_1 <- function(
    fn = rlang::caller_fn(),
    call = rlang::caller_call(),
    call_env = rlang::caller_env()
  ) {
    # Standardize the call arguments
    call_args <- rlang::call_args(rlang::call_match(
      call = call,
      fn = fn,
      defaults = TRUE,
      dots_expand = FALSE
    ))

    # Evaluate the call arguments in the correct environment. If the call
    # contains dots, `call_match()` will have given them as a pairlist so we'll
    # have to evaluate those as well.
    out <- lapply(call_args, eval, call_env)
    dots <- out[["..."]]
    if (!is.null(dots)) {
      out[["..."]] <- lapply(dots, eval, call_env)
    }
    out
  }

  eval_call_args_2 <- function(
    fn = rlang::caller_fn(),
    call = rlang::caller_call(),
    call_env = rlang::caller_env()
  ) {
    # Create a list of the form `list(arg1 = arg1, arg2 = arg2, ... = list2(...))`
    # which is used as the body of argument capturing function.
    fmls_names <- names(formals(fn))
    fmls_syms <- rlang::syms(fmls_names)
    fmls_syms[fmls_names == "..."] <- list(quote(rlang::list2(...)))
    args_list <- rlang::set_names(fmls_syms, fmls_names)

    # Create a new function with the same formals as the `call`'s function `fn`,
    # but which just returns it's evaluated arguments in a list.
    capture_args <- rlang::new_function(
      args = formals(fn),
      body = rlang::expr(list(!!!args_list)),
      env = call_env
    )
    # Evaluate the capturing function with the same arguments as the `call`
    eval(rlang::call2(capture_args, !!!rlang::call_args(call)))
  }

  # Normal usage is fine
  foo <- function(x, ...) {}
  eval_call_args_1(fn = foo, call = quote(foo(12, b = 10, 12 + 4)))
  eval_call_args_2(fn = foo, call = quote(foo(12, b = 10, 12 + 4)))
  eval_call_args_3(fn = foo, call = quote(foo(12, b = 10, 12 + 4)))

  # But out-of-order self-referential arguments don't work in `eval_call_args_1()`
  # because it just evaluates the arguments in order.
  bar <- function(y = x, x = 10 + 10) {}
  try(eval_call_args_1(fn = bar, call = quote(bar())))
  eval_call_args_2(fn = bar, call = quote(bar()))
  eval_call_args_3(fn = bar, call = quote(bar()))
}
