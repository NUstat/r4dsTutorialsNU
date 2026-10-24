# Grade exercise code, working around two gradethis bugs

A drop-in replacement for
[`gradethis::grade_this_code()`](https://rstudio.github.io/gradethis/reference/grade_this_code.html),
used by the tutorials in this package. It changes two things before the
code comparison:

## Usage

``` r
grade_this_code(...)
```

## Arguments

- ...:

  Arguments passed on to
  [`gradethis::grade_this_code()`](https://rstudio.github.io/gradethis/reference/grade_this_code.html),
  such as `correct`.

## Value

A grading function, like the one returned by
[`gradethis::grade_this_code()`](https://rstudio.github.io/gradethis/reference/grade_this_code.html).

## Details

- **`color` arguments.** gradethis 0.2.14 rejects a correct answer when
  an argument named `color` (or containing `color`, such as
  `outlier.color`) appears in a different order than in the solution.
  Its name check sees the student's `color`, while the solution has
  already been renamed to `colour`, the spelling ggplot2 uses
  internally. This function renames `color` to `colour` in argument
  names of both the student's code and the solutions, so the order no
  longer matters.

- **Trailing commas.** Code such as `labs(title = "A", )` makes
  gradethis crash and report a grading problem. This function marks such
  code wrong instead, with a message asking the student to remove the
  trailing comma. `[` and `[[` calls are left alone, because `df[1, ]`
  is not a trailing comma.

- **magrittr pipes.** gradethis 0.2.14 mistakes the string `"."` for the
  magrittr placeholder when it rewrites `%>%`, so a correct answer such
  as `df %>% separate_wider_delim(x, delim = ".", ...)` is rejected.
  This function rewrites `lhs %>% f(...)` as `f(lhs, ...)` (or puts
  `lhs` in place of a bare `.` argument) before comparing, the same way
  R's parser already handles `|>`. Students who use `%>%` are graded the
  same as students who use `|>`.

Only the copies of the code used for grading are changed. The student's
code still runs exactly as typed.

Tutorials use it by adding this line to their setup chunk, after
`gradethis_setup()`, so that every `grade_this_code()` call in their
check chunks uses this version:

    grade_this_code <- r4dsTutorialsNU::grade_this_code
