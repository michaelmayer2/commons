"""Agent-written SAS: the run_sas tool.

What a submission tells the model and the NOXCMD probe are pinned by
``tests/shared/run-sas.json``. The tool tests stand in for SASPy, installing a
fake ``saspy`` module so the agent opens its own session as it would against
a server.
"""

import re
import sys
import types
from typing import Any

import pandas as pd
import pytest
from chatlas import Chat, ContentToolResult, Tool

from commons import SasSession, data_source, sas_session
from commons._agent import Commons
from commons._handles import HandleStore
from commons._prompt import read_prompt
from commons._provenance import TAG_EXTRA_KEY, Tag
from commons._run_sas import (
    MAX_LISTING_CHARS,
    MAX_LOG_CHARS,
    XCMD_PROBE,
    SasLockdownError,
    run_sas_tool,
    run_sas_value,
    xcmd_setting,
)

from ._provider import scripted_chat
from ._shared import load_shared_fixture

FIXTURE = load_shared_fixture("run-sas")
VALUE_CASES: list[dict[str, Any]] = FIXTURE["value"]["cases"]
PROBE_CASES: list[dict[str, Any]] = FIXTURE["probe"]["cases"]


def test_the_run_sas_fixture_is_not_empty() -> None:
    assert VALUE_CASES
    assert PROBE_CASES


def test_the_limits_and_probe_match_the_shared_fixture() -> None:
    assert FIXTURE["value"]["max_listing_chars"] == MAX_LISTING_CHARS
    assert FIXTURE["value"]["max_log_chars"] == MAX_LOG_CHARS
    assert FIXTURE["probe"]["code"] == XCMD_PROBE


def expand(text: str) -> str:
    return re.sub(r"\$repeat:(.):(\d+)", lambda m: m.group(1) * int(m.group(2)), text)


@pytest.mark.parametrize("case", VALUE_CASES, ids=lambda case: case["name"])
def test_run_sas_value_matches_the_shared_fixture(case: dict[str, Any]) -> None:
    value = run_sas_value(expand(case["log"]), expand(case["listing"]))

    assert value == expand(case["expected"])


@pytest.mark.parametrize("case", PROBE_CASES, ids=lambda case: case["name"])
def test_xcmd_setting_matches_the_shared_fixture(case: dict[str, Any]) -> None:
    setting = xcmd_setting(case["log"])

    assert setting == case["expected"]
    assert (setting == "NOXCMD") is case["allowed"]


# ---- the tool --------------------------------------------------------------


class StandInSaspy:
    """Answers a SASPy session's calls, as a server with ``xcmd`` would."""

    def __init__(self, xcmd: str = "NOXCMD", log: str = "", listing: str = ""):
        self.xcmd = xcmd
        self.log = log
        self.listing = listing
        self.submitted: list[str] = []
        self.tables: dict[str, Any] = {}

    def submit(self, code: str, results: str = "HTML") -> dict[str, str]:
        assert results == "TEXT"
        self.submitted.append(code)
        if code == XCMD_PROBE:
            return {"LOG": f"1    {code}COMMONS_XCMD={self.xcmd}\n", "LST": ""}
        return {"LOG": self.log, "LST": self.listing}

    def exist(self, table: str, libref: str = "WORK") -> bool:
        return f"{libref}.{table}" in self.tables

    def sd2df(self, table: str, libref: str = "WORK") -> Any:
        return self.tables[f"{libref}.{table}"]

    def df2sd(self, df: Any, table: str = "_df", libref: str = "WORK") -> None:
        self.tables[f"{libref}.{table}"] = df


def call(tool: Tool, code: str) -> ContentToolResult:
    result = tool.func(code=code)
    assert isinstance(result, ContentToolResult)
    return result


def test_run_sas_reports_the_listing_with_the_b_tag() -> None:
    saspy = StandInSaspy(
        log="NOTE: There were 19 observations read from the data set SASHELP.CLASS.\n",
        listing="  Mean\n  62.3\n",
    )
    tool = run_sas_tool(SasSession(session=saspy), HandleStore())

    result = call(tool, "proc means data=sashelp.class; var height; run;")

    assert result.value.startswith(
        "Output:\n  Mean\n  62.3\n\nLog:\nNOTE: There were 19"
    )
    assert result.extra[TAG_EXTRA_KEY] is Tag.B
    assert saspy.submitted == [
        XCMD_PROBE,
        "proc means data=sashelp.class; var height; run;",
    ]


def test_run_sas_refuses_a_session_that_allows_host_commands() -> None:
    saspy = StandInSaspy(xcmd="XCMD")
    tool = run_sas_tool(SasSession(session=saspy), HandleStore())

    with pytest.raises(SasLockdownError, match="NOXCMD"):
        call(tool, "x 'rm -rf /';")
    assert saspy.submitted == [XCMD_PROBE]


def test_run_sas_probes_the_session_once() -> None:
    saspy = StandInSaspy()
    tool = run_sas_tool(SasSession(session=saspy), HandleStore())

    call(tool, "run;")
    call(tool, "run;")

    assert saspy.submitted.count(XCMD_PROBE) == 1


def test_run_sas_uploads_each_data_frame_handle_once() -> None:
    saspy = StandInSaspy()
    handles = HandleStore()
    handles.register(pd.DataFrame({"region": ["EMEA"], "revenue": [500.0]}))
    handles.register(42)
    tool = run_sas_tool(SasSession(session=saspy), handles)

    call(tool, "proc print data=work.r1; run;")
    handles.register(pd.DataFrame({"x": [1]}))
    call(tool, "proc print data=work.r3; run;")

    assert sorted(saspy.tables) == ["WORK.r1", "WORK.r3"]


def test_run_sas_describes_itself_with_the_shared_prompt() -> None:
    tool = run_sas_tool(SasSession(session=StandInSaspy()), HandleStore())

    assert tool.schema["function"]["description"] == read_prompt("run-sas-tool.md")


# ---- the agent -------------------------------------------------------------


@pytest.fixture
def client() -> Chat:
    return scripted_chat()


@pytest.fixture
def fake_saspy(monkeypatch: pytest.MonkeyPatch) -> list[StandInSaspy]:
    """Install a ``saspy`` module whose sessions are stand-ins, and list them."""
    opened: list[StandInSaspy] = []

    def SASsession(**options: Any) -> StandInSaspy:
        session = StandInSaspy(listing=f"opened with {options}")
        opened.append(session)
        return session

    module = types.ModuleType("saspy")
    module.SASsession = SASsession  # type: ignore[attr-defined]
    monkeypatch.setitem(sys.modules, "saspy", module)
    return opened


def test_an_agent_without_sas_has_no_run_sas(client: Chat) -> None:
    agent = Commons(client, data_source(sales=pd.DataFrame({"a": [1]})))

    assert "run_sas" not in [tool.name for tool in agent.get_tools()]
    assert "run_sas" not in (agent.system_prompt or "")


def test_an_agent_with_sas_runs_agent_code_in_a_session_of_its_own(
    client: Chat, fake_saspy: list[StandInSaspy]
) -> None:
    trusted = sas_session("oda")
    agent = Commons(client, data_source(sales=pd.DataFrame({"a": [1]})), sas=trusted)
    tool = next(tool for tool in agent.get_tools() if tool.name == "run_sas")

    assert fake_saspy == []  # nothing connects until the tool runs
    assert "Code, listings, and log messages from `run_sas`." in (
        agent.system_prompt or ""
    )

    assert isinstance(tool, Tool)
    result = call(tool, "proc print data=sashelp.class(obs=1); run;")

    assert result.value.startswith("Output:\nopened with {'cfgname': 'oda'}\n\n")
    assert "<commons-citation>" in result.value  # ad hoc, so citations are asked for
    assert len(fake_saspy) == 1
    assert trusted._session is None  # the trusted session stayed closed


def test_an_agent_needs_a_session_it_can_open_again(client: Chat) -> None:
    with pytest.raises(ValueError, match="separate one"):
        Commons(
            client,
            data_source(sales=pd.DataFrame({"a": [1]})),
            sas=SasSession(session=StandInSaspy()),
        )


def test_an_agent_refuses_a_sas_that_is_not_a_session(client: Chat) -> None:
    with pytest.raises(TypeError, match="SasSession"):
        Commons(client, data_source(sales=pd.DataFrame({"a": [1]})), sas="oda")  # type: ignore[arg-type]
