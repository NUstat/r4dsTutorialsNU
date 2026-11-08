#' Grade exercise code, working around gradethis bugs
#'
#' A drop-in replacement for [gradethis::grade_this_code()], used by the
#' tutorials in this package. It changes these things before the code
#' comparison:
#'
#' * **`color` arguments.** gradethis 0.2.14 rejects a correct answer when an
#'   argument named `color` (or containing `color`, such as `outlier.color`)
#'   appears in a different order than in the solution. Its name check sees the
#'   student's `color`, while the solution has already been renamed to
#'   `colour`, the spelling ggplot2 uses internally. This function renames
#'   `color` to `colour` in argument names of both the student's code and the
#'   solutions, so the order no longer matters.
#' * **Trailing commas.** Code such as `labs(title = "A", )` makes gradethis
#'   crash and report a grading problem. This function marks such code wrong
#'   instead, with a message asking the student to remove the trailing comma.
#'   `[` and `[[` calls are left alone, because `df[1, ]` is not a trailing
#'   comma.
#' * **Repeated argument names.** When a call repeats an argument name a
#'   different number of times than the solution does, such as the same line
#'   twice inside `fct_recode()`, gradethis 0.2.14 means to mark it wrong, but
#'   it crashes and reports a grading problem whenever that call sits inside
#'   another call, as in `mutate(x = fct_recode(...))`. And when the count is
#'   right, gradethis checks only the first argument with that name, so a
#'   mistake in the third `"Other" = ` line of `fct_recode()` is marked
#'   correct. This function checks the count and every value itself, and marks
#'   code wrong with a message naming the argument to fix. Names the solution
#'   repeats, such as `age` twice in `mutate()`, are fine when the student's
#'   code repeats them the same way.
#' * **magrittr pipes.** gradethis 0.2.14 mistakes the string `"."` for the
#'   magrittr placeholder when it rewrites `%>%`, so a correct answer such as
#'   `df %>% separate_wider_delim(x, delim = ".", ...)` is rejected. This
#'   function rewrites `lhs %>% f(...)` as `f(lhs, ...)` (or puts `lhs` in place
#'   of a bare `.` argument) before comparing, the same way R's parser already
#'   handles `|>`. Students who use `%>%` are graded the same as students who
#'   use `|>`.
#'
#' Only the copies of the code used for grading are changed. The student's
#' code still runs exactly as typed.
#'
#' Tutorials use it by adding this line to their setup chunk, after
#' `gradethis_setup()`, so that every `grade_this_code()` call in their check
#' chunks uses this version:
#'
#' ```r
#' grade_this_code <- r4dsTutorialsNU::grade_this_code
#' ```
#'
#' @param ... Arguments passed on to [gradethis::grade_this_code()], such as
#'   `correct`.
#'
#' @return A grading function, like the one returned by
#'   [gradethis::grade_this_code()].
#'
#' @export
grade_this_code <- function(...) {
  checker <- gradethis::grade_this_code(...)

  function(check_env) {
    user_code <- check_env[[".user_code"]]
    solutions <- check_env[[".solution_code_all"]]
    if (is.null(solutions)) {
      solutions <- check_env[[".solution_code"]]
    }

    message <- NULL
    comma_call <- find_trailing_comma(user_code)
    if (!is.null(comma_call)) {
      message <- paste0(
        "Your call to `", comma_call, "()` ends with a comma right before ",
        "the closing parenthesis. Remove that trailing comma and try again."
      )
    } else if (length(solutions) > 0) {
      message <- find_repeated_names(user_code, unlist(solutions), check_env)
    }
    if (!is.null(message)) {
      # fail() runs its message through glue, so escape any braces
      check_env[[".__problem_message"]] <- gsub("([{}])", "\\1\\1", message)
      return(gradethis::grade_this(
        gradethis::fail(get(".__problem_message"), hint = FALSE)
      )(check_env))
    }

    check_env[[".user_code"]] <- standardise_code(user_code)
    if (!is.null(check_env[[".solution_code"]])) {
      check_env[[".solution_code"]] <- standardise_code(check_env[[".solution_code"]])
    }
    solutions <- check_env[[".solution_code_all"]]
    if (!is.null(solutions)) {
      solutions[] <- lapply(solutions, standardise_code)
      check_env[[".solution_code_all"]] <- solutions
    }

    checker(check_env)
  }
}

# Parse code, returning NULL when it is not a single non-empty string or does
# not parse (gradethis then reports the problem itself).
parse_code <- function(code) {
  if (!is.character(code) || length(code) != 1 || !nzchar(code)) {
    return(NULL)
  }
  tryCatch(parse(text = code, keep.source = FALSE), error = function(e) NULL)
}

is_empty_arg <- function(x) identical(x, quote(expr = ))

is_subset_call <- function(x) {
  is.symbol(x[[1]]) && as.character(x[[1]]) %in% c("[", "[[")
}

arg_names <- function(x) {
  nm <- names(x)
  if (is.null(nm)) rep("", length(x)) else nm
}

# Name of the first function called with a trailing empty argument, or NULL.
find_trailing_comma <- function(code) {
  exprs <- parse_code(code)
  if (is.null(exprs)) {
    return(NULL)
  }
  walk <- function(x) {
    if (!is.call(x)) {
      return(NULL)
    }
    n <- length(x)
    if (n > 1 && is_empty_arg(x[[n]]) && !nzchar(arg_names(x)[n]) &&
        !is_subset_call(x)) {
      return(paste(deparse(x[[1]]), collapse = ""))
    }
    for (i in seq_len(n)) {
      if (!is_empty_arg(x[[i]]) && is.call(x[[i]])) {
        found <- walk(x[[i]])
        if (!is.null(found)) {
          return(found)
        }
      }
    }
    NULL
  }
  for (expr in exprs) {
    found <- walk(expr)
    if (!is.null(found)) {
      return(found)
    }
  }
  NULL
}

# Every call in the code, nested calls included, as a list.
all_calls <- function(code) {
  calls <- list()
  walk <- function(x) {
    if (!is.call(x)) {
      return()
    }
    calls[[length(calls) + 1]] <<- x
    for (i in seq_along(x)) {
      if (!is_empty_arg(x[[i]]) && is.call(x[[i]])) {
        walk(x[[i]])
      }
    }
  }
  for (expr in parse_code(code)) {
    walk(expr)
  }
  calls
}

# Name of the function a call uses, without any `pkg::` prefix.
call_name <- function(x) {
  fn <- x[[1]]
  if (is.call(fn) && length(fn) == 3 && is.symbol(fn[[1]]) &&
      as.character(fn[[1]]) %in% c("::", ":::")) {
    fn <- fn[[3]]
  }
  paste(deparse(fn), collapse = "")
}

# Message for the first argument name that the user's code repeats, inside some
# call, without matching a call to the same function in the solutions, or
# NULL. A match has the same number of arguments with that name, and each
# value matches one of them, in any order (gradethis checks the order later).
#
# gradethis 0.2.14 handles repeated names badly in two ways. Its
# detect_name_problems() passes the enclosing call and argument to
# detect_duplicate_names() in the wrong order, which crashes when the count
# differs and the call is nested inside another. And when the count is right,
# it compares only the first argument with each name, so a wrong value in a
# later one, such as the third `"Other" = ` in fct_recode(), passes.
find_repeated_names <- function(user_code, solutions, env) {
  solution_calls <- unlist(lapply(solutions, all_calls), recursive = FALSE)
  for (x in all_calls(user_code)) {
    fn <- call_name(x)
    nm <- arg_names(x)
    for (name in unique(nm[duplicated(nm) & nzchar(nm)])) {
      user_args <- as.list(x)[nm == name]
      expected <- lapply(
        Filter(function(s) identical(call_name(s), fn), solution_calls),
        function(s) as.list(s)[arg_names(s) == name]
      )
      shown <- if (identical(make.names(name), name)) name else paste0('"', name, '"')

      n_user <- length(user_args)
      if (!n_user %in% lengths(expected)) {
        advice <- if (all(lengths(expected) < n_user)) {
          "but it needs fewer. Check whether you repeated a line or misspelled an argument name."
        } else {
          "but it needs more. Check whether you left one out."
        }
        return(paste0(
          "Your call to `", fn, "()` has ", n_user, " arguments named `", shown,
          "`, ", advice
        ))
      }

      unmatched <- lapply(
        expected[lengths(expected) == n_user],
        function(s) unmatched_arg(user_args, s, env)
      )
      if (any(vapply(unmatched, is.null, logical(1)))) {
        next
      }
      value <- paste(deparse(unmatched[[1]][[1]], width.cutoff = 500L), collapse = " ")
      wrong <- if (nchar(value) <= 60) {
        paste0("the argument `", shown, " = ", value, "` is")
      } else {
        paste0("one of the arguments named `", shown, "` is")
      }
      return(paste0(
        "In your call to `", fn, "()`, ", wrong, " wrong or repeated. ",
        "Check each argument named `", shown, "`."
      ))
    }
  }
  NULL
}

# The first user argument that matches none of the solution arguments, each
# solution argument matching at most once, wrapped in a list (the argument
# itself may be NULL). Returns NULL when every user argument has a match.
unmatched_arg <- function(user_args, solution_args, env) {
  for (u in user_args) {
    hit <- Position(function(s) same_code(u, s, env), solution_args)
    if (is.na(hit)) {
      return(list(u))
    }
    solution_args <- solution_args[-hit]
  }
  NULL
}

# Whether two expressions are the same code by gradethis's standards, so that
# `if_else(x, "a", "b")` matches `if_else(x, true = "a", false = "b")`. When
# gradethis cannot compare them, they count as the same, leaving the decision
# to the full comparison that runs afterwards.
same_code <- function(a, b, env) {
  if (identical(a, b)) {
    return(TRUE)
  }
  feedback <- tryCatch(
    gradethis::code_feedback(
      paste(deparse(a, width.cutoff = 500L), collapse = "\n"),
      paste(deparse(b, width.cutoff = 500L), collapse = "\n"),
      user_env = env,
      solution_env = env
    ),
    error = function(e) NULL
  )
  is.null(feedback)
}

# Rewrite magrittr pipes as the calls they stand for: `lhs %>% f(a)` becomes
# `f(lhs, a)`, `lhs %>% f(a, .)` becomes `f(a, lhs)`, and `lhs %>% f` becomes
# `f(lhs)`. Inner pipes are rewritten first, so chains work. Braced or
# parenthesised right-hand sides are left alone.
unpipe_magrittr <- function(x) {
  if (!is.call(x)) {
    return(x)
  }
  for (i in seq_along(x)) {
    if (!is_empty_arg(x[[i]]) && is.call(x[[i]])) {
      x[[i]] <- unpipe_magrittr(x[[i]])
    }
  }
  if (!identical(x[[1]], as.name("%>%")) || length(x) != 3) {
    return(x)
  }
  lhs <- x[[2]]
  rhs <- x[[3]]
  if (is.symbol(rhs)) {
    return(as.call(list(rhs, lhs)))
  }
  if (!is.call(rhs) || identical(rhs[[1]], as.name("{")) || identical(rhs[[1]], as.name("("))) {
    return(x)
  }
  if (length(rhs) > 1) {
    is_dot <- vapply(2:length(rhs), function(i) identical(rhs[[i]], as.name(".")), logical(1))
    if (any(is_dot)) {
      rhs[[which(is_dot)[1] + 1]] <- lhs
      return(rhs)
    }
  }
  as.call(c(list(rhs[[1]], lhs), as.list(rhs)[-1]))
}

# Prepare code for comparison: rewrite magrittr pipes, then rename `color` to
# `colour` inside argument names. Returns the code unchanged when there is
# nothing to change or it does not parse.
standardise_code <- function(code) {
  exprs <- parse_code(code)
  if (is.null(exprs)) {
    return(code)
  }
  changed <- FALSE
  walk <- function(x) {
    if (!is.call(x)) {
      return(x)
    }
    if (identical(x[[1]], as.name("%>%"))) {
      x <- unpipe_magrittr(x)
      changed <<- TRUE
      if (!is.call(x)) {
        return(x)
      }
    }
    nm <- names(x)
    if (!is.null(nm) && any(grepl("color", nm, fixed = TRUE))) {
      names(x) <- gsub("color", "colour", nm, fixed = TRUE)
      changed <<- TRUE
    }
    # Recurse only into calls: assigning NULL (e.g. from `name = NULL`) into a
    # call would delete that element.
    for (i in seq_along(x)) {
      if (!is_empty_arg(x[[i]]) && is.call(x[[i]])) {
        x[[i]] <- walk(x[[i]])
      }
    }
    x
  }
  exprs <- lapply(exprs, walk)
  if (!changed) {
    return(code)
  }
  paste(
    vapply(exprs, function(e) paste(deparse(e, width.cutoff = 500L), collapse = "\n"), ""),
    collapse = "\n"
  )
}
