"""Trusted SAS code: reading .sas measures, binding arguments, and running them.

The header grammar, the prelude, and the log check are pinned by
``tests/shared/sas-measures.json``. Running a measure needs a SAS server, so
the end-to-end tests use a small stand-in for a SASPy session, and the live
test runs only when ``COMMONS_TEST_SAS_CFGNAME`` names a SASPy configuration.
"""

import os
from pathlib import Path
from typing import Any

import pandas as pd
import pytest

from commons import SasSession, data_source, sas_measures, semantic_layer
from commons._measures import measure_schema_text
from commons._provenance import TAG_EXTRA_KEY, Tag
from commons._sas import (
    SasArgument,
    SasMeasureError,
    SasRunError,
    parse_sas_measures,
    sas_log_errors,
    sas_measure,
    sas_prelude,
)
from commons._tools import ToolContext, build_commons_tools

from ._shared import load_shared_fixture

FIXTURE = load_shared_fixture("sas-measures")
PARSE_CASES: list[dict[str, Any]] = FIXTURE["parse"]["cases"]
ERROR_CASES: list[dict[str, Any]] = FIXTURE["parse"]["errors"]
SCHEMA_CASES: list[dict[str, Any]] = FIXTURE["schema"]["cases"]
PRELUDE_CASES: list[dict[str, Any]] = FIXTURE["prelude"]["cases"]
LOG_CASES: list[dict[str, Any]] = FIXTURE["log_errors"]["cases"]


def test_the_sas_fixture_is_not_empty() -> None:
    for cases in (PARSE_CASES, ERROR_CASES, SCHEMA_CASES, PRELUDE_CASES, LOG_CASES):
        assert cases


def _argument_record(argument: SasArgument) -> dict[str, Any]:
    record: dict[str, Any] = {"name": argument.name, "type": argument.type}
    if argument.type == "enum":
        record["values"] = list(argument.values)
    if argument.items is not None:
        items: dict[str, Any] = {"type": argument.items.type}
        if argument.items.type == "enum":
            items["values"] = list(argument.items.values)
        record["items"] = items
    record["required"] = argument.required
    if argument.default is not None:
        record["default"] = argument.default
    record["description"] = argument.description
    return record


def _fixture_argument(spec: dict[str, Any]) -> SasArgument:
    items = spec.get("items")
    return SasArgument(
        name=spec.get("name", ""),
        type=spec["type"],
        required=spec.get("required", True),
        values=tuple(spec.get("values", ())),
        items=_fixture_argument(items) if items else None,
    )


@pytest.mark.parametrize("case", PARSE_CASES, ids=lambda case: case["name"])
def test_parse_sas_measures_matches_the_shared_fixture(case: dict[str, Any]) -> None:
    parsed = parse_sas_measures(case["text"], case["stem"])

    records = [
        {
            "name": spec.name,
            "title": spec.title,
            "description": spec.description,
            "arguments": [_argument_record(argument) for argument in spec.arguments],
            "output": (
                {"libref": spec.output[0], "table": spec.output[1]}
                if spec.output
                else None
            ),
            "provenance": list(spec.provenance),
            "code": spec.code,
        }
        for spec in parsed
    ]
    assert records == case["expected"]


@pytest.mark.parametrize("case", ERROR_CASES, ids=lambda case: case["name"])
def test_parse_sas_measures_refuses_the_shared_fixture_errors(
    case: dict[str, Any],
) -> None:
    with pytest.raises(SasMeasureError) as error:
        parse_sas_measures(case["text"], case["stem"])

    assert error.value.code == case["error"]


@pytest.mark.parametrize("case", SCHEMA_CASES, ids=lambda case: case["name"])
def test_a_sas_measure_renders_the_shared_schema_text(case: dict[str, Any]) -> None:
    (spec,) = parse_sas_measures(case["text"], case["stem"])

    assert measure_schema_text(sas_measure(spec)) == case["expected"]


@pytest.mark.parametrize("case", PRELUDE_CASES, ids=lambda case: case["name"])
def test_sas_prelude_matches_the_shared_fixture(case: dict[str, Any]) -> None:
    output = case["output"]
    prelude = sas_prelude(
        [_fixture_argument(argument) for argument in case["arguments"]],
        (output["libref"], output["table"]) if output else None,
        case["values"],
    )

    assert prelude == case["expected"]


@pytest.mark.parametrize("case", LOG_CASES, ids=lambda case: case["name"])
def test_sas_log_errors_matches_the_shared_fixture(case: dict[str, Any]) -> None:
    assert sas_log_errors(case["log"]) == case["expected"]


# ---- running ---------------------------------------------------------------


class StandInSaspy:
    """Answers the four SASPy calls a SAS measure makes, without SAS."""

    def __init__(self, log: str = "NOTE: ok\n", tables: dict[str, Any] | None = None):
        self.log = log
        self.tables = tables or {}
        self.submitted: list[str] = []

    def submit(self, code: str, results: str = "HTML") -> dict[str, str]:
        assert results == "TEXT"
        self.submitted.append(code)
        return {"LOG": self.log, "LST": "The SAS System\n\n  N\n 19\n"}

    def exist(self, table: str, libref: str = "WORK") -> bool:
        return f"{libref}.{table}" in self.tables

    def sd2df(self, table: str, libref: str = "WORK") -> Any:
        return self.tables[f"{libref}.{table}"]

    def df2sd(self, df: Any, table: str = "_df", libref: str = "WORK") -> None:
        self.tables[f"{libref}.{table}"] = df


HEIGHTS = """\
/**
 * Mean height for one sex
 * @measure
 * @param sex `enum[F, M]` Which sex.
 * @param [min_age=11] `integer` Youngest age included.
 * @provenance https://example.com/heights.sas
 */
proc means data=sashelp.class(where=(sex = "&sex" and age >= &min_age)) noprint;
  var height; output out=work.result mean=height;
run;
"""


def heights(tmp_path: Path, saspy: StandInSaspy) -> dict[str, Any]:
    path = tmp_path / "heights.sas"
    path.write_text(HEIGHTS)
    layer = semantic_layer(sas_measures(path, session=SasSession(session=saspy)))
    return dict(layer.measures)


def call_measure(measures: dict[str, Any], **kwargs: Any) -> Any:
    tools = build_commons_tools(
        ToolContext(
            sources={"db": data_source(x=pd.DataFrame({"a": [1]}))}, measures=measures
        )
    )
    tool = next(tool for tool in tools if tool.name == "call_measure")
    return tool.func(**kwargs)


def test_a_sas_measure_runs_through_call_measure_with_the_a_tag(tmp_path: Path) -> None:
    saspy = StandInSaspy(tables={"WORK.RESULT": pd.DataFrame({"height": [60.5886]})})
    measures = heights(tmp_path, saspy)

    result = call_measure(measures, name="heights", arguments='{"sex": "F"}')

    assert "60.5886" in result.value
    assert result.extra[TAG_EXTRA_KEY] is Tag.A
    (submitted,) = saspy.submitted
    assert submitted.startswith(
        "data _null_;\n  call symputx('sex', 'F', 'G');\n"
        "  call symputx('min_age', '11', 'G');\nrun;\n"
    )
    assert submitted.endswith("output out=work.result mean=height;\nrun;")


def test_a_sas_measure_refuses_a_value_outside_its_enum(tmp_path: Path) -> None:
    saspy = StandInSaspy()
    measures = heights(tmp_path, saspy)

    with pytest.raises(Exception, match="sex"):
        call_measure(measures, name="heights", arguments='{"sex": "X"}')
    assert saspy.submitted == []


def test_a_sas_error_fails_the_measure(tmp_path: Path) -> None:
    # A stale table from an earlier run must not be returned as this run's.
    saspy = StandInSaspy(
        log="ERROR: File SASHELP.CLASS.DATA does not exist.\n",
        tables={"WORK.RESULT": pd.DataFrame({"height": [1.0]})},
    )
    measures = heights(tmp_path, saspy)

    with pytest.raises(SasRunError, match="SASHELP.CLASS.DATA"):
        call_measure(measures, name="heights", arguments='{"sex": "M"}')


def test_a_missing_output_table_is_an_error() -> None:
    (spec,) = parse_sas_measures(HEIGHTS, "heights")
    record = sas_measure(spec, SasSession(session=StandInSaspy()))

    with pytest.raises(SasRunError, match="WORK.RESULT"):
        record.func(sex="F")


def test_a_sas_measure_without_output_returns_the_listing() -> None:
    text = "/**\n * Class size\n * @measure\n * @output none\n */\nproc sql; select count(*) as n from sashelp.class; quit;\n"
    (spec,) = parse_sas_measures(text, "class_size")

    assert "19" in sas_measure(spec, SasSession(session=StandInSaspy())).func()


def test_a_sas_measures_source_is_its_sas_code(tmp_path: Path) -> None:
    path = tmp_path / "heights.sas"
    path.write_text(HEIGHTS)

    layer = semantic_layer(tmp_path)

    assert list(layer.measures) == ["heights"]
    assert layer.source_text["heights"].startswith("proc means")
    assert layer.measures["heights"].provenance == ("https://example.com/heights.sas",)


def test_a_session_does_not_connect_until_a_measure_runs() -> None:
    session = SasSession("never-used")

    assert "not connected" in repr(session)


@pytest.mark.skipif(
    not os.environ.get("COMMONS_TEST_SAS_CFGNAME"),
    reason="COMMONS_TEST_SAS_CFGNAME names no SASPy configuration",
)
def test_a_sas_measure_runs_on_a_live_sas_session() -> None:
    pytest.importorskip("saspy")
    text = (
        "/**\n * Students of one sex\n * @measure\n * @param sex `enum[F, M]` Sex.\n */\n"
        'data work.result; set sashelp.class; where sex = "&sex"; run;\n'
    )
    (spec,) = parse_sas_measures(text, "students")
    session = SasSession(os.environ["COMMONS_TEST_SAS_CFGNAME"])

    frame = sas_measure(spec, session).func(sex="F")

    assert len(frame) == 9
