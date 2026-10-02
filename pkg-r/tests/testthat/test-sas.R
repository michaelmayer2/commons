# Trusted SAS code. The header grammar, the prelude, and the log check are
# pinned by tests/shared/sas-measures.json. Running a measure needs a SAS
# server, so the end-to-end tests use a stand-in session, and the live test
# runs only when COMMONS_TEST_SAS_CFGNAME names a SASPy configuration.

sas_fixture <- shared_fixture("sas-measures")

test_that("the SAS fixture is not empty", {
  expect_gt(length(sas_fixture$parse$cases), 0)
  expect_gt(length(sas_fixture$parse$errors), 0)
  expect_gt(length(sas_fixture$schema$cases), 0)
  expect_gt(length(sas_fixture$prelude$cases), 0)
  expect_gt(length(sas_fixture$log_errors$cases), 0)
})

sas_argument_record <- function(argument) {
  record <- list(name = argument$name, type = argument$type)
  if (identical(argument$type, "enum")) {
    record$values <- as.list(argument$values)
  }
  if (!is.null(argument$items)) {
    items <- list(type = argument$items$type)
    if (identical(argument$items$type, "enum")) {
      items$values <- as.list(argument$items$values)
    }
    record$items <- items
  }
  record$required <- argument$required
  if (!is.null(argument$default)) {
    record$default <- argument$default
  }
  record$description <- argument$description
  record
}

sas_fixture_argument <- function(spec) {
  argument <- new_sas_argument(
    spec$name %||% "",
    spec$type,
    required = spec$required %||% TRUE,
    values = unlist(spec$values) %||% character()
  )
  if (!is.null(spec$items)) {
    argument$items <- sas_fixture_argument(spec$items)
  }
  argument
}

for (case in sas_fixture$parse$cases) {
  test_that(paste("parse:", case$name), {
    parsed <- parse_sas_measures(case$text, case$stem)
    records <- lapply(parsed, function(spec) {
      list(
        name = spec$name,
        title = spec$title,
        description = spec$description,
        arguments = lapply(spec$arguments, sas_argument_record),
        output = if (!is.null(spec$output)) as.list(spec$output),
        provenance = as.list(spec$provenance),
        code = spec$code
      )
    })
    expect_equal(records, case$expected)
  })
}

for (case in sas_fixture$parse$errors) {
  test_that(paste("parse error:", case$name), {
    error <- expect_error(
      parse_sas_measures(case$text, case$stem),
      class = "commons_sas_measure_error"
    )
    expect_equal(error$code, case$error)
  })
}

for (case in sas_fixture$schema$cases) {
  test_that(paste("schema:", case$name), {
    spec <- parse_sas_measures(case$text, case$stem)[[1]]
    expect_equal(measure_schema_text(sas_measure(spec)), case$expected)
  })
}

for (case in sas_fixture$prelude$cases) {
  test_that(paste("prelude:", case$name), {
    output <- if (!is.null(case$output)) unlist(case$output)
    prelude <- sas_prelude(
      lapply(case$arguments, sas_fixture_argument),
      output,
      case$values
    )
    expect_equal(prelude, case$expected)
  })
}

for (case in sas_fixture$log_errors$cases) {
  test_that(paste("log errors:", case$name), {
    expect_equal(sas_log_errors(case$log), as.character(unlist(case$expected)))
  })
}

# ---- running -----------------------------------------------------------------

stand_in_sas <- function(log = "NOTE: ok\n", tables = list()) {
  state <- new.env()
  state$submitted <- character()
  state$tables <- tables
  session <- new_sas_session(list(
    submit = function(code) {
      state$submitted <- c(state$submitted, code)
      list(log = log, listing = "The SAS System\n\n  N\n 19\n")
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
  list(session = session, state = state)
}

heights_sas <- c(
  "/**",
  " * Mean height for one sex",
  " * @measure",
  " * @param sex `enum[F, M]` Which sex.",
  " * @param [min_age=11] `integer` Youngest age included.",
  " * @provenance https://example.com/heights.sas",
  " */",
  "proc means data=sashelp.class(where=(sex = \"&sex\" and age >= &min_age)) noprint;",
  "  var height; output out=work.result mean=height;",
  "run;"
)

heights_layer <- function(sas) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  path <- file.path(dir, "heights.sas")
  writeLines(heights_sas, path)
  semantic_layer_state(semantic_layer(sas_measures(
    path,
    session = sas$session
  )))
}

test_that("a SAS measure runs through call_measure with the A tag", {
  sas <- stand_in_sas(tables = list(WORK.RESULT = data.frame(height = 60.5886)))
  layer <- heights_layer(sas)

  res <- call_measure_tool(layer$measures, "heights", '{"sex": "F"}')

  expect_match(res@value, "60.5886", fixed = TRUE)
  expect_equal(res@extra$commons_tag, "A")
  expect_length(sas$state$submitted, 1)
  expect_true(startsWith(
    sas$state$submitted,
    paste0(
      "data _null_;\n  call symputx('sex', 'F', 'G');\n",
      "  call symputx('min_age', '11', 'G');\nrun;\n"
    )
  ))
  expect_true(endsWith(
    sas$state$submitted,
    "output out=work.result mean=height;\nrun;"
  ))
})

test_that("a SAS measure refuses a value outside its enum", {
  sas <- stand_in_sas()
  layer <- heights_layer(sas)

  expect_error(
    call_measure_tool(layer$measures, "heights", '{"sex": "X"}'),
    "sex"
  )
  expect_length(sas$state$submitted, 0)
})

test_that("a SAS error fails the measure", {
  # A stale table from an earlier run must not be returned as this run's.
  sas <- stand_in_sas(
    log = "ERROR: File SASHELP.CLASS.DATA does not exist.\n",
    tables = list(WORK.RESULT = data.frame(height = 1))
  )
  layer <- heights_layer(sas)

  expect_error(
    call_measure_tool(layer$measures, "heights", '{"sex": "M"}'),
    "SASHELP.CLASS.DATA",
    class = "commons_sas_run_error"
  )
})

test_that("a missing output table is an error", {
  spec <- parse_sas_measures(paste(heights_sas, collapse = "\n"), "heights")[[
    1
  ]]
  record <- sas_measure(spec, stand_in_sas()$session)

  expect_error(
    record(sex = "F"),
    "WORK.RESULT",
    class = "commons_sas_run_error"
  )
})

test_that("a SAS measure without output returns the listing", {
  text <- "/**\n * Class size\n * @measure\n * @output none\n */\nproc sql; select count(*) as n from sashelp.class; quit;\n"
  spec <- parse_sas_measures(text, "class_size")[[1]]

  expect_match(sas_measure(spec, stand_in_sas()$session)(), "19")
})

test_that("semantic_layer() reads .sas files beside .R files", {
  dir <- withr::local_tempdir()
  writeLines(heights_sas, file.path(dir, "heights.sas"))
  writeLines(
    c("#' Order count", "#' @measure", "order_count <- function() 2"),
    file.path(dir, "orders.R")
  )

  layer <- semantic_layer_state(semantic_layer(dir))

  expect_setequal(names(layer$measures), c("heights", "order_count"))
  expect_equal(
    layer$measure_provenance$heights,
    "https://example.com/heights.sas"
  )
  expect_match(layer$fn_sources[["heights"]], "# proc means", fixed = TRUE)
  # The run_r worker evaluates every source, so a SAS measure's must parse.
  expect_no_error(parse(text = layer$fn_sources[["heights"]]))
})

test_that("a session does not connect until a measure runs", {
  expect_snapshot(sas_session("never-used"))
})

test_that("a SAS measure runs on a live SAS session", {
  cfgname <- Sys.getenv("COMMONS_TEST_SAS_CFGNAME")
  skip_if(
    !nzchar(cfgname),
    "COMMONS_TEST_SAS_CFGNAME names no SASPy configuration"
  )
  skip_if_not_installed("sasquatch")
  text <- paste0(
    "/**\n * Students of one sex\n * @measure\n * @param sex `enum[F, M]` Sex.\n */\n",
    "data work.result; set sashelp.class; where sex = \"&sex\"; run;\n"
  )
  spec <- parse_sas_measures(text, "students")[[1]]

  frame <- sas_measure(spec, sas_session(cfgname))(sex = "F")

  expect_equal(nrow(frame), 9)
})
