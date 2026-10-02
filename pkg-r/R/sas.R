#' Connect to SAS for trusted SAS measures
#'
#' `sas_session()` describes the SAS connection that SAS measures run on. It
#' does not connect: the connection opens through
#' [sasquatch](https://docs.ropensci.org/sasquatch/), and so through SASPy, the
#' first time a SAS measure runs. That means you can build a semantic layer of
#' SAS measures before SAS is reachable.
#'
#' sasquatch holds one SAS session per R process. If a session is already open,
#' for instance from [sasquatch::sas_connect()], SAS measures reuse it whatever
#' its configuration.
#'
#' @param cfgname The name of a SASPy configuration, as listed in
#'   `sascfg_personal.py`. If `NULL`, SASPy's default configuration is used.
#'   See `vignette("configuration", package = "sasquatch")`.
#'
#' @return A `commons_sas_session` object to pass to [sas_measures()].
#'
#' @seealso [sas_measures()] to read measures from `.sas` files.
#'
#' @examples
#' sas_session("oda")
#'
#' @export
sas_session <- function(cfgname = NULL) {
  rlang::check_string(cfgname, allow_null = TRUE)
  new_sas_session(sasquatch_backend(cfgname), cfgname = cfgname)
}

# A session is the four operations a SAS measure needs, so another route to
# SAS can stand in for sasquatch, and optionally a way to open a separate
# session on the same configuration, for agent-written SAS.
new_sas_session <- function(backend, cfgname = NULL) {
  structure(
    list(
      submit = backend$submit,
      table_exists = backend$table_exists,
      to_df = backend$to_df,
      from_df = backend$from_df,
      separate = backend$separate,
      cfgname = cfgname
    ),
    class = "commons_sas_session"
  )
}

#' @export
print.commons_sas_session <- function(x, ...) {
  config <- if (is.null(x$cfgname)) "the default configuration" else x$cfgname
  cli::cli_text("A SAS session using {config}, opened on first use.")
  invisible(x)
}

sasquatch_backend <- function(cfgname) {
  connect <- function() {
    rlang::check_installed(
      c("sasquatch", "reticulate"),
      reason = "to run SAS measures."
    )
    session <- sasquatch::sas_get_session()
    if (is.null(session) || is.null(session$SASpid)) {
      if (is.null(cfgname)) {
        sasquatch::sas_connect()
      } else {
        sasquatch::sas_connect(cfgname)
      }
      session <- sasquatch::sas_get_session()
    }
    session
  }

  list(
    submit = function(code) {
      result <- py_value(connect()$submit(code, results = "TEXT"))
      list(
        log = paste(result$LOG, collapse = "\n"),
        listing = paste(result$LST, collapse = "\n")
      )
    },
    table_exists = function(table, libref = "WORK") {
      isTRUE(py_value(connect()$exist(table, libref)))
    },
    to_df = function(table, libref = "WORK") {
      connect()
      sasquatch::sas_to_r(table, libref)
    },
    from_df = function(df, table, libref = "WORK") {
      connect()
      sasquatch::sas_from_r(df, table, libref)
    },
    separate = function() {
      new_sas_session(saspy_backend(cfgname), cfgname = cfgname)
    }
  )
}

py_value <- function(x) {
  if (inherits(x, "python.builtin.object")) reticulate::py_to_r(x) else x
}

the_sas_session <- new.env(parent = emptyenv())

default_sas_session <- function() {
  the_sas_session$session <- the_sas_session$session %||% sas_session()
  the_sas_session$session
}

#' Read trusted SAS measures
#'
#' `sas_measures()` reads measures from `.sas` files, for [semantic_layer()].
#' Each becomes a measure like any other: [commons()] runs it through the
#' same tool, validates its arguments the same way, and marks its answers
#' as verified. A `.sas` path given to [semantic_layer()] directly runs on a
#' default [sas_session()]. Use `sas_measures()` to choose the session.
#'
#' @section Measure blocks:
#' A measure is declared in a comment block that opens with `/**` at the start
#' of a line and holds an `@measure` tag. Its code is everything after the
#' block, up to the next measure block or the end of the file. Code before the
#' first measure block, such as macro definitions, runs before every measure
#' in the file.
#'
#' ```sas
#' /**
#'  * Mean height for one sex
#'  *
#'  * Averages SASHELP.CLASS heights.
#'  *
#'  * @measure heights
#'  * @param sex `enum[F, M]` Which sex.
#'  * @param [min_age=11] `integer` Youngest age included.
#'  * @return One row with the mean height.
#'  * @output WORK.RESULT
#'  * @provenance https://example.com/heights.sas
#'  */
#' proc means data=sashelp.class(where=(sex = "&sex" and age >= &min_age));
#'   var height; output out=work.result mean=height;
#' run;
#' ```
#'
#' The first paragraph is the title, and any further paragraphs and the
#' `@return` text describe the measure. `@measure` can name the measure;
#' otherwise it is named after the file.
#'
#' Each `@param` declares an argument the model supplies. Brackets mark an
#' optional argument and can give a default (`[year=2025]`). A type code can
#' follow: `string` (the default), `integer`, `number`, `boolean`,
#' `enum[value, ...]`, or any of these followed by `[]` for an array.
#'
#' `@output` names the dataset the measure creates, which is returned as a data
#' frame: `LIBREF.TABLE`, or `TABLE` in `WORK`. The default is `WORK.RESULT`.
#' `@output none` returns the listing instead. `@provenance` links to where
#' the code came from, and can be repeated.
#'
#' @section How a measure runs:
#' Each argument is set as a global macro variable with `CALL SYMPUTX` before
#' the code runs. Every declared argument is set on every call, and an unset
#' optional one is empty, so a value from an earlier call never leaks into a
#' later one. Strings and enums are the value itself, booleans are `1` or `0`,
#' and arrays are their elements separated by spaces, with strings
#' single-quoted, ready for `where x in (&name)`. Refer to a string argument
#' as `%superq(name)` to keep SAS from resolving macro triggers in it.
#'
#' The output dataset is deleted before the code runs. A log line starting with
#' `ERROR` fails the measure, and so does a run that leaves no output dataset.
#'
#' @param paths Paths to `.sas` files or directories containing them.
#'   Directory searches are not recursive.
#' @param session The [sas_session()] the measures run on. If `NULL`, a default
#'   session using SASPy's default configuration.
#'
#' @return Measures to pass to [semantic_layer()].
#'
#' @examples
#' path <- tempfile(fileext = ".sas")
#' writeLines(c(
#'   "/**",
#'   " * Number of students",
#'   " * @measure class_size",
#'   " * @output none",
#'   " */",
#'   "proc sql; select count(*) from sashelp.class; quit;"
#' ), path)
#' semantic_layer(sas_measures(path, session = sas_session("oda")))
#'
#' @export
sas_measures <- function(paths, session = NULL) {
  if (!is.character(paths)) {
    cli::cli_abort(
      "{.arg paths} must be a character vector of file or directory paths."
    )
  }
  if (!is.null(session) && !inherits(session, "commons_sas_session")) {
    cli::cli_abort("{.arg session} must be a {.fn sas_session} or `NULL`.")
  }
  files <- resolve_measure_files(paths)
  read_sas_measure_files(files[is_sas_file(files)], session)
}

is_sas_file <- function(files) {
  grepl("[.]sas$", files, ignore.case = TRUE)
}

read_sas_measure_files <- function(files, session = NULL) {
  specs <- unlist(
    lapply(files, function(file) {
      text <- paste(
        readLines(file, warn = FALSE, encoding = "UTF-8"),
        collapse = "\n"
      )
      stem <- sub("[.][^.]*$", "", basename(file))
      parse_sas_measures(text, stem)
    }),
    recursive = FALSE
  ) %||%
    list()

  fn_sources <- vapply(specs, sas_fn_source, character(1))
  names(fn_sources) <- vapply(specs, `[[`, character(1), "name")
  new_measure_files(
    measures = lapply(specs, sas_measure, session = session),
    fn_sources = fn_sources,
    provenance = lapply(specs, `[[`, "provenance"),
    measure_display = lapply(specs, function(spec) {
      list(description = spec$display_description, details = spec$details)
    })
  )
}

# What the agent's R session holds under a SAS measure's name: a function
# whose printed source is the SAS it runs, since the session cannot run SAS.
sas_fn_source <- function(spec) {
  lines <- strsplit(spec$code, "\n", fixed = TRUE)[[1]]
  paste0(
    "function(...) {\n",
    "  # A trusted SAS measure. Its SAS code:\n",
    paste0("  # ", lines, collapse = "\n"),
    "\n  stop(\"",
    spec$name,
    " is a SAS measure; run it with call_measure.\")\n",
    "}"
  )
}

combine_measure_files <- function(...) {
  bundles <- list(...)
  fn_sources <- unlist(lapply(bundles, `[[`, "fn_sources")) %||% character()
  new_measure_files(
    measures = do.call(c, lapply(bundles, `[[`, "measures")) %||% list(),
    fn_sources = fn_sources,
    provenance = do.call(c, lapply(bundles, `[[`, "provenance")) %||% list(),
    measure_display = do.call(c, lapply(bundles, `[[`, "measure_display")) %||%
      list()
  )
}

# ---- Header parsing ----------------------------------------------------------
#
# The grammar is a cross-language contract pinned by
# tests/shared/sas-measures.json; pkg-py/src/commons/_sas.py reads it too.

sas_scalar_kinds <- c("string", "integer", "number", "boolean")
sas_known_tags <- c("measure", "param", "return", "output", "provenance")
sas_macro_name_re <- "^[A-Za-z_][A-Za-z0-9_]{0,31}$"

sas_measure_abort <- function(code, message, call = rlang::caller_env()) {
  cli::cli_abort(
    message,
    class = "commons_sas_measure_error",
    code = code,
    call = call,
    .envir = parent.frame()
  )
}

parse_sas_measures <- function(text, stem) {
  text <- gsub("\r\n", "\n", text, fixed = TRUE)
  match <- gregexpr("(?ms)^[ \\t]*/\\*\\*(.*?)\\*/", text, perl = TRUE)[[1]]
  if (match[[1]] == -1) {
    return(list())
  }

  starts <- as.integer(match)
  ends <- starts + attr(match, "match.length")
  body_starts <- attr(match, "capture.start")[, 1]
  bodies <- substring(
    text,
    body_starts,
    body_starts + attr(match, "capture.length")[, 1] - 1
  )
  is_measure <- vapply(bodies, is_sas_measure_block, logical(1))
  if (!any(is_measure)) {
    return(list())
  }
  starts <- starts[is_measure]
  ends <- ends[is_measure]
  bodies <- bodies[is_measure]

  preamble <- trim_sas_code(substr(text, 1, starts[[1]] - 1))
  code_ends <- c(starts[-1] - 1, nchar(text))
  specs <- list()
  for (i in seq_along(bodies)) {
    code <- trim_sas_code(substr(text, ends[[i]], code_ends[[i]]))
    if (nzchar(preamble)) {
      code <- paste0(preamble, "\n\n", code)
    }
    spec <- parse_sas_block(bodies[[i]], stem, code)
    if (spec$name %in% vapply(specs, `[[`, character(1), "name")) {
      name <- spec$name
      sas_measure_abort(
        "duplicate-name",
        c(
          "Two measures in {.file {stem}.sas} are named {.val {name}}.",
          i = "Name each with {.code @measure <name>}."
        )
      )
    }
    specs[[length(specs) + 1]] <- spec
  }
  specs
}

sas_block_lines <- function(body) {
  lines <- strsplit(body, "\n", fixed = TRUE)[[1]]
  trimws(sub("^\\*", "", trimws(lines, "left")))
}

sas_tag_re <- "^@(\\w+)\\s*(.*)$"

is_sas_measure_block <- function(body) {
  lines <- sas_block_lines(body)
  tags <- regmatches(lines, regexec(sas_tag_re, lines, perl = TRUE))
  any(vapply(
    tags,
    function(m) length(m) > 0 && m[[2]] == "measure",
    logical(1)
  ))
}

trim_sas_code <- function(code) {
  code <- sub("^(?:[ \\t]*\\n)+", "", code, perl = TRUE)
  sub("\\s+$", "", code, perl = TRUE)
}

parse_sas_block <- function(body, stem, code) {
  paragraphs <- list(character())
  tags <- list()
  for (line in sas_block_lines(body)) {
    tag <- regmatches(line, regexec(sas_tag_re, line, perl = TRUE))[[1]]
    if (length(tag)) {
      text <- if (nzchar(tag[[3]])) tag[[3]] else character()
      tags[[length(tags) + 1]] <- list(tag = tag[[2]], text = text)
    } else if (!nzchar(line)) {
      if (length(tags)) {
        # A blank line ends a tag's text.
        tags[[length(tags) + 1]] <- list(tag = "", text = character())
      } else if (length(paragraphs[[length(paragraphs)]])) {
        paragraphs[[length(paragraphs) + 1]] <- character()
      }
    } else if (length(tags)) {
      n <- length(tags)
      tags[[n]]$text <- c(tags[[n]]$text, line)
    } else {
      n <- length(paragraphs)
      paragraphs[[n]] <- c(paragraphs[[n]], line)
    }
  }

  prose <- vapply(
    Filter(length, paragraphs),
    paste,
    character(1),
    collapse = " "
  )
  name <- NULL
  arguments <- list()
  returns <- NULL
  output <- c(libref = "WORK", table = "RESULT")
  provenance <- character()

  for (entry in tags) {
    tag <- entry$tag
    value <- trimws(paste(entry$text, collapse = " "))
    if (!nzchar(tag)) {
      next
    }
    if (!tag %in% sas_known_tags) {
      sas_measure_abort(
        "unknown-tag",
        c(
          "Unknown tag {.code @{tag}} in {.file {stem}.sas}.",
          i = "Measure blocks use {.code @measure}, {.code @param}, {.code @return}, {.code @output}, and {.code @provenance}."
        )
      )
    }
    switch(
      tag,
      measure = {
        if (nzchar(value)) name <- value
      },
      param = {
        argument <- parse_sas_param(value, stem)
        seen <- tolower(vapply(arguments, `[[`, character(1), "name"))
        if (tolower(argument$name) %in% seen) {
          arg_name <- argument$name
          sas_measure_abort(
            "duplicate-param",
            "Argument {.arg {arg_name}} is declared twice in {.file {stem}.sas}."
          )
        }
        arguments[[length(arguments) + 1]] <- argument
      },
      return = {
        returns <- value
      },
      output = {
        output <- parse_sas_output(value, stem)
      },
      provenance = {
        provenance <- c(provenance, value)
      }
    )
  }

  name <- name %||% stem
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", name)) {
    sas_measure_abort(
      "invalid-name",
      c(
        "{.val {name}} is not a valid measure name.",
        i = "Use letters, digits, and underscores, or name it with {.code @measure <name>}."
      )
    )
  }
  if (!length(prose)) {
    sas_measure_abort(
      "no-description",
      c(
        "Measure {.val {name}} in {.file {stem}.sas} has no title.",
        i = "Start its header block with a line saying what it computes."
      )
    )
  }

  details <- if (!is.null(returns)) paste0("Returns: ", returns)
  list(
    name = name,
    title = prose[[1]],
    description = paste(c(prose, details), collapse = "\n\n"),
    display_description = paste(prose[-1], collapse = "\n\n"),
    details = details,
    arguments = arguments,
    output = output,
    provenance = provenance,
    code = code
  )
}

parse_sas_param <- function(text, stem) {
  m <- regmatches(
    text,
    regexec("(?s)^(\\[[^]]*\\]|\\S+)\\s*(.*)$", text, perl = TRUE)
  )[[1]]
  if (!length(m)) {
    sas_measure_abort(
      "invalid-param",
      "Empty {.code @param} in {.file {stem}.sas}."
    )
  }
  head <- m[[2]]
  rest <- m[[3]]

  required <- TRUE
  default_text <- NULL
  if (startsWith(head, "[")) {
    required <- FALSE
    head <- trimws(substr(head, 2, nchar(head) - 1))
    if (grepl("=", head, fixed = TRUE)) {
      default_text <- trimws(sub("^[^=]*=", "", head))
      head <- trimws(sub("=.*$", "", head))
    }
  }
  if (!grepl(sas_macro_name_re, head)) {
    sas_measure_abort(
      "invalid-param",
      "{.val {head}} in {.file {stem}.sas} is not a SAS macro variable name."
    )
  }

  type_code <- "string"
  typed <- regmatches(
    rest,
    regexec("(?s)^`([^`]*)`\\s*(.*)$", rest, perl = TRUE)
  )[[1]]
  if (length(typed)) {
    type_code <- trimws(typed[[2]])
    rest <- typed[[3]]
  }
  argument <- parse_sas_type(type_code, head, trimws(rest), required, stem)

  if (!is.null(default_text)) {
    default <- parse_sas_default(argument, default_text)
    if (is.null(default)) {
      sas_measure_abort(
        "invalid-default",
        "Default {.val {default_text}} for {.arg {head}} in {.file {stem}.sas} does not fit its type."
      )
    }
    argument$default <- default
  }
  argument
}

new_sas_argument <- function(
  name,
  type,
  description = "",
  required = TRUE,
  values = character(),
  items = NULL
) {
  list(
    name = name,
    type = type,
    description = description,
    required = required,
    values = values,
    items = items,
    default = NULL
  )
}

parse_sas_type <- function(code, name, description, required, stem) {
  if (endsWith(code, "[]")) {
    items <- parse_sas_type(
      trimws(substr(code, 1, nchar(code) - 2)),
      name,
      "",
      TRUE,
      stem
    )
    if (identical(items$type, "array")) {
      sas_measure_abort(
        "invalid-param",
        "Nested arrays for {.arg {name}} in {.file {stem}.sas}."
      )
    }
    return(new_sas_argument(
      name,
      "array",
      description,
      required,
      items = items
    ))
  }
  enum <- regmatches(code, regexec("^enum\\[([^]]*)\\]$", code, perl = TRUE))[[
    1
  ]]
  if (length(enum)) {
    values <- trimws(strsplit(enum[[2]], ",", fixed = TRUE)[[1]])
    return(new_sas_argument(
      name,
      "enum",
      description,
      required,
      values = values
    ))
  }
  if (code %in% sas_scalar_kinds) {
    return(new_sas_argument(name, code, description, required))
  }
  sas_measure_abort(
    "invalid-param",
    c(
      "Unknown type {.code {code}} for {.arg {name}} in {.file {stem}.sas}.",
      i = "Use string, integer, number, boolean, enum[...], or an array of one of them."
    )
  )
}

parse_sas_default <- function(argument, text) {
  switch(
    argument$type,
    string = text,
    enum = if (text %in% argument$values) text,
    integer = if (grepl("^[+-]?[0-9]+$", text)) as.integer(text),
    number = {
      value <- suppressWarnings(as.numeric(text))
      if (!is.na(value)) value
    },
    boolean = switch(
      tolower(text),
      true = ,
      `1` = TRUE,
      false = ,
      `0` = FALSE,
      NULL
    ),
    NULL
  )
}

parse_sas_output <- function(text, stem) {
  if (identical(tolower(text), "none")) {
    return(NULL)
  }
  parts <- strsplit(text, ".", fixed = TRUE)[[1]]
  libref <- if (length(parts) > 1)
    paste(parts[-length(parts)], collapse = ".") else "WORK"
  table <- parts[length(parts)]
  if (
    !length(parts) ||
      !grepl("^[A-Za-z_][A-Za-z0-9_]{0,7}$", libref) ||
      !grepl(sas_macro_name_re, table)
  ) {
    sas_measure_abort(
      "invalid-output",
      c(
        "{.code @output {text}} in {.file {stem}.sas} is not a SAS dataset name.",
        i = "Use LIBREF.TABLE, TABLE (in WORK), or none."
      )
    )
  }
  c(libref = toupper(libref), table = toupper(table))
}

# ---- Running -----------------------------------------------------------------

sas_literal <- function(x) {
  paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
}

sas_macro_text <- function(argument, value) {
  if (is.null(value)) {
    return("")
  }
  switch(
    argument$type,
    array = {
      items <- argument$items
      value <- unlist(value)
      if (items$type %in% c("string", "enum")) {
        paste(sas_literal(as.character(value)), collapse = " ")
      } else {
        paste(
          vapply(
            value,
            function(item) sas_macro_text(items, item),
            character(1)
          ),
          collapse = " "
        )
      }
    },
    boolean = if (isTRUE(as.logical(value))) "1" else "0",
    integer = sprintf("%d", as.integer(value)),
    number = sprintf("%.15g", as.numeric(value)),
    as.character(value)
  )
}

sas_prelude <- function(arguments, output, values) {
  lines <- character()
  if (length(arguments)) {
    assignments <- vapply(
      arguments,
      function(argument) {
        text <- sas_macro_text(argument, values[[argument$name]])
        sprintf(
          "  call symputx(%s, %s, 'G');",
          sas_literal(argument$name),
          sas_literal(text)
        )
      },
      character(1)
    )
    lines <- c("data _null_;", assignments, "run;")
  }
  if (!is.null(output)) {
    lines <- c(
      lines,
      sprintf("proc datasets lib=%s nolist nowarn;", output[["libref"]]),
      sprintf("  delete %s;", output[["table"]]),
      "quit;"
    )
  }
  paste0(lines, "\n", collapse = "", recycle0 = TRUE)
}

sas_log_errors <- function(log) {
  lines <- strsplit(log, "\n", fixed = TRUE)[[1]]
  errors <- lines[grepl("^ERROR(\\s+[0-9]+-[0-9]+)?:", lines, perl = TRUE)]
  sub("\\s+$", "", errors, perl = TRUE)
}

sas_run_measure <- function(spec, session, values) {
  session <- session %||% default_sas_session()
  code <- paste0(sas_prelude(spec$arguments, spec$output, values), spec$code)
  result <- session$submit(code)

  errors <- sas_log_errors(result$log)
  if (length(errors)) {
    names(errors) <- rep("x", length(errors))
    cli::cli_abort(
      c(
        "SAS reported errors running measure {.val {spec$name}}.",
        utils::head(errors, 5)
      ),
      class = "commons_sas_run_error"
    )
  }
  if (is.null(spec$output)) {
    return(result$listing)
  }
  libref <- spec$output[["libref"]]
  table <- spec$output[["table"]]
  if (!isTRUE(session$table_exists(table, libref))) {
    cli::cli_abort(
      c(
        "Measure {.val {spec$name}} ran without creating {.code {libref}.{table}}.",
        i = "Its code must create the table named by {.code @output}."
      ),
      class = "commons_sas_run_error"
    )
  }
  session$to_df(table, libref)
}

sas_argument_type <- function(argument) {
  required <- isTRUE(argument$required)
  description <- argument$description
  switch(
    argument$type,
    enum = ellmer::type_enum(
      values = argument$values,
      description = description,
      required = required
    ),
    array = {
      items <- argument$items
      item_type <- if (identical(items$type, "enum")) {
        ellmer::type_enum(values = items$values)
      } else {
        scalar_type(items$type, "")
      }
      ellmer::type_array(
        items = item_type,
        description = description,
        required = required
      )
    },
    scalar_type(argument$type, description, required = required)
  )
}

# The measure's function takes the model's arguments as formals, so ellmer and
# validate_measure_args() treat it like any other measure. The spec and session
# live in its closure under names no SAS macro variable can have.
sas_measure <- function(spec, session = NULL) {
  arg_names <- vapply(spec$arguments, `[[`, character(1), "name")
  formals <- rep(list(quote(expr = )), length(arg_names))
  names(formals) <- arg_names
  for (i in seq_along(spec$arguments)) {
    argument <- spec$arguments[[i]]
    if (!isTRUE(argument$required)) {
      formals[i] <- list(argument$default)
    }
  }

  env <- new.env(parent = asNamespace("commons"))
  env$.commons_sas_spec <- spec
  env$.commons_sas_session <- session
  fn <- rlang::new_function(
    formals,
    quote(sas_run_measure(
      .commons_sas_spec,
      .commons_sas_session,
      as.list(environment())
    )),
    env
  )

  arguments <- lapply(spec$arguments, sas_argument_type)
  names(arguments) <- arg_names
  measure(
    spec$name,
    spec$description,
    fn,
    arguments = arguments,
    title = spec$title
  )
}
