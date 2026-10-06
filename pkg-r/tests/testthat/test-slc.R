# SAS-language code on Altair SLC, through slcR. An SLC session answers the
# calls a SAS session does, so SAS measures and run_sas run on it unchanged.
# Without SLC installed, the tests use a stand-in for slcR's Slc; the live
# test runs only when SLC is installed.

stand_in_slc <- function() {
  started <- new.env()
  started$processes <- list()
  start <- function(sys_options) {
    slc <- new.env()
    slc$sys_options <- sys_options
    slc$submitted <- character()
    slc$log <- character()
    slc$listing <- ""
    slc$tables <- list()
    slc$submit <- function(code) {
      slc$submitted <- c(slc$submitted, code)
      slc$log <- c(slc$log, paste0("1    ", code))
      if (identical(code, sas_xcmd_probe)) {
        slc$log <- c(slc$log, "COMMONS_XCMD=NOXCMD")
      } else if (grepl("work.result", code, fixed = TRUE)) {
        slc$tables$WORK.RESULT <- data.frame(n = 9)
        slc$log <- c(slc$log, "NOTE: The data set WORK.RESULT has 1 observations.")
      } else {
        slc$listing <- "The SLC System\n\n  N\n 19"
      }
      0L
    }
    # The log accumulates across submissions, as slcR's does.
    slc$get_log <- function() paste(slc$log, collapse = "\n")
    slc$get_listing_output <- function() slc$listing
    slc$clear_listing_output <- function() slc$listing <- ""
    slc$get_library <- function(name = "WORK") {
      key <- function(table) toupper(paste0(name, ".", table))
      list(
        get_dataset_names = function() {
          tables <- names(slc$tables)
          sub("^[^.]*[.]", "", tables[startsWith(tables, paste0(name, "."))])
        },
        get_dataset_as_dataframe = function(table) slc$tables[[key(table)]],
        create_dataset_from_dataframe = function(df, table) {
          slc$tables[[key(table)]] <- df
        }
      )
    }
    started$processes <- c(started$processes, list(slc))
    slc
  }
  session <- new_sas_session(slcr_backend(list(), start), engine = "SLC")
  list(session = session, started = started)
}

students_sas <- c(
  "/**",
  " * Students of one sex",
  " * @measure",
  " * @param sex `enum[F, M]` Sex.",
  " */",
  "data work.result; set work.class; where sex = \"&sex\"; run;"
)

test_that("slc_session() starts nothing until it is used", {
  expect_snapshot(slc_session(list(ENCODING = "UTF-8")))
  expect_error(slc_session(list("UTF-8")), "named list")
})

test_that("a SAS measure runs on SLC", {
  slc <- stand_in_slc()
  spec <- parse_sas_measures(paste(students_sas, collapse = "\n"), "students")[[1]]

  frame <- sas_measure(spec, slc$session)(sex = "F")

  expect_equal(frame$n, 9)
  process <- slc$started$processes[[1]]
  expect_true(startsWith(
    process$submitted[[1]],
    "data _null_;\n  call symputx('sex', 'F', 'G');"
  ))
})

test_that("each SLC submission sees only its own log", {
  slc <- stand_in_slc()

  first <- slc$session$submit("proc print data=work.class; run;")
  second <- slc$session$submit("data work.result; run;")

  expect_equal(first$listing, "The SLC System\n\n  N\n 19")
  expect_no_match(first$log, "WORK.RESULT", fixed = TRUE)
  expect_equal(
    second$log,
    "1    data work.result; run;\nNOTE: The data set WORK.RESULT has 1 observations."
  )
  expect_equal(second$listing, "")
})

test_that("an agent runs agent code in an SLC process of its own", {
  slc <- stand_in_slc()
  agent <- test_agent(sas = slc$session)

  expect_length(slc$started$processes, 0)
  res <- agent_tool(agent, "run_sas")(code = "proc print data=work.class; run;")

  expect_true(startsWith(res@value, "Output:\nThe SLC System"))
  expect_equal(res@extra$commons_tag, "B")
  # The trusted session never started; the agent's own did.
  expect_length(slc$started$processes, 1)
  expect_equal(slc$started$processes[[1]]$submitted[[1]], sas_xcmd_probe)
})

test_that("a live SLC process runs agent code", {
  skip_if_not_installed("slcR", "0.3.3")
  skip_if(
    !nzchar(Sys.getenv("WPSHOME")) && !dir.exists("/opt/altair/slc/2026"),
    "Altair SLC is not installed."
  )
  agent <- test_agent(sas = slc_session())

  res <- agent_tool(agent, "run_sas")(
    code = "data work.one; x = 1; run; proc print data=work.one; run;"
  )

  expect_true(startsWith(res@value, "Output:"))
})

test_that("a SAS measure runs on a live SLC process", {
  skip_if_not_installed("slcR", "0.3.3")
  skip_if(
    !nzchar(Sys.getenv("WPSHOME")) && !dir.exists("/opt/altair/slc/2026"),
    "Altair SLC is not installed."
  )
  session <- slc_session()
  session$submit(paste(
    "data work.class; input name $ sex $; datalines;",
    "Alice F",
    "Bob M",
    "Carol F",
    ";",
    "run;",
    sep = "\n"
  ))
  spec <- parse_sas_measures(paste(students_sas, collapse = "\n"), "students")[[1]]

  frame <- sas_measure(spec, session)(sex = "F")

  expect_equal(nrow(frame), 2)
})
