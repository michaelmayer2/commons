# Agent-written SAS: the run_sas tool. What a submission tells the model and
# the NOXCMD probe are pinned by tests/shared/run-sas.json. The tool tests use
# a stand-in for the agent's SAS session, as a server would answer it.

run_sas_fixture <- shared_fixture("run-sas")

test_that("the run_sas fixture is not empty", {
  expect_gt(length(run_sas_fixture$value$cases), 0)
  expect_gt(length(run_sas_fixture$probe$cases), 0)
  expect_gt(length(run_sas_fixture$slc_options$cases), 0)
})

test_that("the limits and probe match the shared fixture", {
  expect_equal(run_sas_fixture$value$max_listing_chars, run_sas_max_listing_chars)
  expect_equal(run_sas_fixture$value$max_log_chars, run_sas_max_log_chars)
  expect_equal(run_sas_fixture$probe$code, sas_xcmd_probe)
  expect_equal(run_sas_fixture$slc_options$defaults, slc_default_options)
})

expand_repeats <- function(text) {
  matches <- regmatches(text, gregexpr("\\$repeat:(.):([0-9]+)", text))[[1]]
  for (match in matches) {
    parts <- regmatches(match, regexec("\\$repeat:(.):([0-9]+)", match))[[1]]
    text <- sub(
      match,
      strrep(parts[[2]], as.integer(parts[[3]])),
      text,
      fixed = TRUE
    )
  }
  text
}

for (case in run_sas_fixture$value$cases) {
  test_that(paste("value:", case$name), {
    value <- run_sas_value(expand_repeats(case$log), expand_repeats(case$listing))
    expect_equal(value, expand_repeats(case$expected))
  })
}

for (case in run_sas_fixture$probe$cases) {
  test_that(paste("probe:", case$name), {
    setting <- sas_xcmd_setting(case$log)
    expect_equal(setting, case$expected)
    expect_equal(identical(setting, "NOXCMD"), case$allowed)
  })
}

for (case in run_sas_fixture$slc_options$cases) {
  test_that(paste("SLC options:", case$name), {
    expect_equal(slc_options(case$given), case$expected)
  })
}

# ---- the tool --------------------------------------------------------------

stand_in_agent_sas <- function(xcmd = "NOXCMD", log = "", listing = "") {
  state <- new.env()
  state$submitted <- character()
  state$tables <- list()
  state$opened <- 0L
  open <- function() {
    state$opened <- state$opened + 1L
    new_sas_session(list(
      submit = function(code) {
        state$submitted <- c(state$submitted, code)
        if (identical(code, sas_xcmd_probe)) {
          return(list(
            log = paste0("1    ", code, "COMMONS_XCMD=", xcmd, "\n"),
            listing = ""
          ))
        }
        list(log = log, listing = listing)
      },
      table_exists = function(table, libref = "WORK") {
        paste0(libref, ".", table) %in% names(state$tables)
      },
      to_df = function(table, libref = "WORK") {
        state$tables[[paste0(libref, ".", table)]]
      },
      from_df = function(df, table, libref = "WORK") {
        state$tables[[paste0(libref, ".", table)]] <- df
      }
    ))
  }
  # The trusted session submits nothing; agent code goes to the one it opens.
  trusted <- new_sas_session(list(
    submit = function(code) stop("agent code reached the trusted session"),
    separate = open
  ))
  list(session = trusted, state = state)
}

sas_agent <- function(sas, ...) {
  test_agent(sas = sas$session, ...)
}

test_that("run_sas reports the listing with the B tag", {
  sas <- stand_in_agent_sas(
    log = "NOTE: There were 19 observations read from the data set SASHELP.CLASS.\n",
    listing = "  Mean\n  62.3\n"
  )
  run_sas <- agent_tool(sas_agent(sas), "run_sas")

  res <- run_sas(code = "proc means data=sashelp.class; var height; run;")

  expect_true(startsWith(
    res@value,
    "Output:\n  Mean\n  62.3\n\nLog:\nNOTE: There were 19"
  ))
  expect_match(res@value, "<commons-citation>", fixed = TRUE)
  expect_equal(res@extra$commons_tag, "B")
  expect_equal(
    sas$state$submitted,
    c(sas_xcmd_probe, "proc means data=sashelp.class; var height; run;")
  )
  expect_equal(sas$state$opened, 1L)
})

test_that("run_sas refuses a session that allows host commands", {
  sas <- stand_in_agent_sas(xcmd = "XCMD")
  run_sas <- agent_tool(sas_agent(sas), "run_sas")

  expect_error(
    run_sas(code = "x 'rm -rf /';"),
    "NOXCMD",
    class = "commons_sas_lockdown_error"
  )
  expect_equal(sas$state$submitted, sas_xcmd_probe)
})

test_that("run_sas probes the session once", {
  sas <- stand_in_agent_sas()
  run_sas <- agent_tool(sas_agent(sas), "run_sas")

  run_sas(code = "run;")
  run_sas(code = "run;")

  expect_equal(sum(sas$state$submitted == sas_xcmd_probe), 1L)
})

test_that("run_sas uploads each data-frame handle once", {
  sas <- stand_in_agent_sas()
  handles <- new_handle_store()
  register_handle(handles, data.frame(region = "EMEA", revenue = 500))
  register_handle(handles, 42)
  agent <- new_agent_sas(sas$session$separate(), handles)

  agent_sas_submit(agent, "proc print data=work.r1; run;")
  register_handle(handles, data.frame(x = 1))
  agent_sas_submit(agent, "proc print data=work.r3; run;")

  expect_equal(sort(names(sas$state$tables)), c("WORK.r1", "WORK.r3"))
})

test_that("run_sas describes itself with the shared prompt", {
  run_sas <- agent_tool(sas_agent(stand_in_agent_sas()), "run_sas")

  expect_equal(tool_description(run_sas), read_prompt("run-sas-tool.md"))
})

test_that("run_sas escapes code and output in its card", {
  html <- run_sas_html("%put <b>;", "", "a < b")

  expect_match(html, "%put &lt;b&gt;;", fixed = TRUE)
  expect_match(html, "a &lt; b", fixed = TRUE)
  expect_match(html, "language-sas", fixed = TRUE)
})

# ---- the agent -------------------------------------------------------------

test_that("an agent without sas has no run_sas", {
  agent <- test_agent()

  expect_false("run_sas" %in% vapply(agent$get_tools(), tool_name, character(1)))
  expect_no_match(agent$get_system_prompt(), "run_sas", fixed = TRUE)
})

test_that("an agent with sas cites run_sas in its prompt", {
  sas <- stand_in_agent_sas()
  agent <- sas_agent(sas)

  expect_match(
    agent$get_system_prompt(),
    "Code, listings, and log messages from `run_sas`.",
    fixed = TRUE
  )
  expect_length(sas$state$submitted, 0) # nothing runs until the tool does
})

test_that("an agent needs a session it can open again", {
  sas <- stand_in_agent_sas()
  sas$session$separate <- NULL

  expect_error(sas_agent(sas), "separate one")
})

test_that("an agent refuses a sas that is not a session", {
  expect_error(test_agent(sas = "oda"), "sas_session")
})

test_that("sas_session() can open a separate session for agent code", {
  session <- sas_session("oda")

  expect_type(session$separate, "closure")
  expect_s3_class(session$separate(), "commons_sas_session")
})

test_that("run_sas runs on a live SAS server", {
  cfgname <- Sys.getenv("COMMONS_TEST_SAS_CFGNAME")
  skip_if(!nzchar(cfgname), "COMMONS_TEST_SAS_CFGNAME is not set.")
  skip_if_not_installed("sasquatch")

  agent <- new_agent_sas(separate_sas_session(sas_session(cfgname)), NULL)
  result <- agent_sas_submit(
    agent,
    "proc means data=sashelp.class mean; var height; run;"
  )

  expect_match(run_sas_value(result$log, result$listing), "62.3368421")
})
