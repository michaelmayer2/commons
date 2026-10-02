# Agent-written SAS: the run_sas tool.
#
# SAS code runs on the SAS server, beyond the reach of the sandbox that
# contains run_r. So run_sas runs agent code only in a session whose server
# was started with NOXCMD, which shuts off host commands, and only in a session
# of its own: trusted SAS measures never share one with agent code, which could
# otherwise change options, librefs, or macros they rely on.
#
# What a submission tells the model, and the probe that checks NOXCMD, are a
# cross-language contract pinned by tests/shared/run-sas.json. The tool's
# description is prompts/run-sas-tool.md, which both packages ship.

run_sas_max_listing_chars <- 10000L
run_sas_max_log_chars <- 6000L
sas_xcmd_probe <- "%put COMMONS_XCMD=%sysfunc(getoption(XCMD));\n"

# sasquatch holds a single SAS session, which trusted measures use, so the
# agent's session is a SASPy session of its own, opened through the Python
# that sasquatch configures.
saspy_backend <- function(cfgname) {
  state <- new.env(parent = emptyenv())
  connect <- function() {
    if (is.null(state$session)) {
      rlang::check_installed(
        c("sasquatch", "reticulate"),
        reason = "to run agent-written SAS."
      )
      loadNamespace("sasquatch")
      saspy <- reticulate::import("saspy")
      reticulate::py_capture_output(
        state$session <- if (is.null(cfgname)) {
          saspy$SASsession()
        } else {
          saspy$SASsession(cfgname = cfgname)
        }
      )
    }
    state$session
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
      session <- connect()
      reticulate::py_to_r(session$sd2df(table, libref))
    },
    from_df = function(df, table, libref = "WORK") {
      session <- connect()
      df[] <- lapply(df, function(col) if (is.factor(col)) as.character(col) else col)
      reticulate::py_capture_output(
        session$df2sd(reticulate::r_to_py(df), table = table, libref = libref)
      )
      invisible(df)
    }
  )
}

separate_sas_session <- function(sas) {
  if (is.null(sas$separate)) {
    cli::cli_abort(c(
      "{.arg sas} must be a session {.fn sas_session} or {.fn slc_session} describes.",
      i = "commons runs agent-written SAS in a session of its own, and cannot open a separate one from this session."
    ))
  }
  sas$separate()
}

# The ERROR, WARNING, and NOTE messages of a SAS log, one string each.
sas_log_messages <- function(log) {
  lines <- sub("\\s+$", "", strsplit(log, "\r?\n")[[1]])
  messages <- list()
  current <- 0L
  for (line in lines) {
    if (grepl("^(ERROR|WARNING|NOTE)(\\s+[0-9]+-[0-9]+)?:", line)) {
      current <- length(messages) + 1L
      messages[[current]] <- line
    } else if (current > 0L && grepl("^[ \t]", line) && nzchar(trimws(line))) {
      messages[[current]] <- c(messages[[current]], line)
    } else {
      current <- 0L
    }
  }
  process_time <- vapply(
    messages,
    function(message) {
      startsWith(message[[1]], "NOTE") &&
        endsWith(message[[1]], "used (Total process time):")
    },
    logical(1)
  )
  vapply(
    messages[!process_time],
    function(message) paste(message, collapse = "\n"),
    character(1)
  )
}

sas_listing_text <- function(listing) {
  lines <- strsplit(gsub("\f", "", listing, fixed = TRUE), "\r?\n")[[1]]
  text <- paste(sub("\\s+$", "", lines), collapse = "\n")
  sub("\\s+$", "", sub("^\n+", "", text))
}

sas_cap <- function(text, limit, what) {
  if (nchar(text) <= limit) {
    return(text)
  }
  sprintf("%s\n[%s truncated.]", substr(text, 1L, limit), what)
}

# What the model is told one submission produced.
run_sas_value <- function(log, listing) {
  messages <- sas_log_messages(log)
  text <- sas_listing_text(listing)
  parts <- c(
    if (any(startsWith(messages, "ERROR"))) {
      "SAS reported errors. Fix the code and run it again."
    },
    if (nzchar(text)) {
      paste0("Output:\n", sas_cap(text, run_sas_max_listing_chars, "Output"))
    },
    if (length(messages)) {
      paste0(
        "Log:\n",
        sas_cap(paste(messages, collapse = "\n"), run_sas_max_log_chars, "Log")
      )
    }
  )
  if (!length(parts)) {
    return("(The code ran but produced no output.)")
  }
  paste(parts, collapse = "\n\n")
}

# The XCMD setting the probe printed, or NULL if it printed none.
sas_xcmd_setting <- function(log) {
  lines <- strsplit(log, "\r?\n")[[1]]
  pattern <- "^COMMONS_XCMD=(\\w+)\\s*$"
  matched <- lines[grepl(pattern, lines, perl = TRUE)]
  if (!length(matched)) {
    return(NULL)
  }
  toupper(sub(pattern, "\\1", matched[[length(matched)]], perl = TRUE))
}

run_sas_html <- function(code, log, listing) {
  parts <- c(sas_listing_text(listing), paste(sas_log_messages(log), collapse = "\n"))
  output <- paste(parts[nzchar(parts)], collapse = "\n\n")
  as.character(htmltools::div(
    class = "commons-run-r-display",
    htmltools::tags$pre(
      class = "commons-run-r-code",
      htmltools::tags$code(class = "language-sas", code)
    ),
    if (nzchar(output)) {
      htmltools::tags$pre(class = "commons-run-r-code", htmltools::tags$code(output))
    }
  ))
}

# The agent's own SAS session, checked once and kept in step with handles:
# each data-frame result is uploaded once, as the WORK dataset named after its
# handle, before the first submission after it.
new_agent_sas <- function(session, handles) {
  agent <- new.env(parent = emptyenv())
  agent$session <- session
  agent$handles <- handles
  agent$checked <- FALSE
  agent$synced <- 0L
  agent
}

agent_sas_submit <- function(agent, code) {
  agent_sas_check(agent)
  ids <- handle_ids(agent$handles)
  for (id in ids[seq_along(ids) > agent$synced]) {
    value <- get_handle(agent$handles, id)
    if (is.data.frame(value)) {
      agent$session$from_df(as.data.frame(value), id, "WORK")
    }
  }
  agent$synced <- length(ids)
  agent$session$submit(code)
}

agent_sas_check <- function(agent) {
  if (agent$checked) {
    return(invisible())
  }
  setting <- sas_xcmd_setting(agent$session$submit(sas_xcmd_probe)$log)
  if (!identical(setting, "NOXCMD")) {
    found <- if (is.null(setting)) {
      "did not report its setting"
    } else {
      paste("reports", setting)
    }
    cli::cli_abort(
      paste(
        "run_sas is unavailable: the SAS session {found}, and commons runs",
        "agent-written SAS only on a server started with NOXCMD.",
        "Tell the user that analysis in SAS is not available."
      ),
      class = "commons_sas_lockdown_error"
    )
  }
  agent$checked <- TRUE
  invisible()
}

run_sas_result <- function(code, result) {
  tool_result(
    run_sas_value(result$log, result$listing),
    title = "Analyzed data",
    icon = maybe_icon("terminal"),
    html = run_sas_html(code, result$log, result$listing),
    tag = "B",
    show_tag = FALSE
  )
}

tool_run_sas <- function(private) {
  ellmer::tool(
    function(code) {
      result <- agent_sas_submit(private$agent_sas, code)
      add_citation_request(run_sas_result(code, result), private$citation_request)
    },
    read_prompt("run-sas-tool.md"),
    arguments = list(
      code = ellmer::type_string("The SAS code to run.")
    ),
    name = "run_sas",
    annotations = ellmer::tool_annotations(
      title = "Analyzing data",
      icon = maybe_icon("terminal"),
      read_only_hint = FALSE,
      open_world_hint = TRUE
    )
  )
}
