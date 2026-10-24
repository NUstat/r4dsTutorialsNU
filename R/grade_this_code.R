#' Grade exercise code, working around two gradethis bugs
#'
#' A drop-in replacement for [gradethis::grade_this_code()], used by the
#' tutorials in this package. It changes two things before the code
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
    comma_call <- find_trailing_comma(check_env[[".user_code"]])
    if (!is.null(comma_call)) {
      check_env[[".__trailing_comma_message"]] <- paste0(
        "Your call to `", comma_call, "()` ends with a comma right before ",
        "the closing parenthesis. Remove that trailing comma and try again."
      )
      return(gradethis::grade_this(
        gradethis::fail(get(".__trailing_comma_message"), hint = FALSE)
      )(check_env))
    }

    check_env[[".user_code"]] <- standardise_color_args(check_env[[".user_code"]])
    if (!is.null(check_env[[".solution_code"]])) {
      check_env[[".solution_code"]] <- standardise_color_args(check_env[[".solution_code"]])
    }
    solutions <- check_env[[".solution_code_all"]]
    if (!is.null(solutions)) {
      solutions[] <- lapply(solutions, standardise_color_args)
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

# Rename `color` to `colour` inside argument names. Returns the code unchanged
# when there is nothing to rename or it does not parse.
standardise_color_args <- function(code) {
  exprs <- parse_code(code)
  if (is.null(exprs)) {
    return(code)
  }
  changed <- FALSE
  walk <- function(x) {
    if (!is.call(x)) {
      return(x)
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
