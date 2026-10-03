"""SAS-language code on Altair SLC, through slcPy.

An SLC session answers the calls a SAS session does, so SAS measures and
run_sas run on it unchanged. Without SLC installed, the tests stand in for
slcPy, installing fake ``slc`` and ``wpslink`` modules; the live tests run
only when slcPy can start SLC.
"""

import os
import subprocess
import sys
import types
from typing import Any

import pandas as pd
import pytest
from chatlas import ContentToolResult, Tool

from commons import SlcSession, data_source, slc_session
from commons._agent import Commons
from commons._handles import HandleStore
from commons._run_sas import XCMD_PROBE, run_sas_tool
from commons._sas import parse_sas_measures, sas_measure

from ._provider import scripted_chat


class StandInDataset:
    def __init__(self, frame: pd.DataFrame) -> None:
        self.frame = frame
        self.closed = False

    def to_data_frame(self) -> pd.DataFrame:
        return self.frame

    def close(self) -> None:
        self.closed = True


class StandInLibrary:
    def __init__(self, slc: "StandInSlc", name: str) -> None:
        self.slc = slc
        self.name = name

    def exist(self, table: str) -> bool:
        return f"{self.name}.{table}".upper() in self.slc.tables

    def open_dataset(self, table: str, mode: Any) -> StandInDataset:
        return StandInDataset(self.slc.tables[f"{self.name}.{table}".upper()])

    def create_dataset_from_dataframe(
        self, table: str, frame: pd.DataFrame
    ) -> StandInDataset:
        self.slc.tables[f"{self.name}.{table}".upper()] = frame
        return StandInDataset(frame)


class StandInSlc:
    """Answers slcPy's calls as SLC would: its log and listing flush as read."""

    def __init__(self, options: list[Any]) -> None:
        self.options = options
        self.submitted: list[str] = []
        self.tables: dict[str, pd.DataFrame] = {}
        self.log: list[str] = []
        self.listing: list[str] = []
        self.xcmd = "NOXCMD"

    def submit(self, code: str) -> int:
        self.submitted.append(code)
        self.log.append(f"1    {code}")
        if code == XCMD_PROBE:
            self.log.append(f"COMMONS_XCMD={self.xcmd}")
        elif "work.result" in code:
            self.tables["WORK.RESULT"] = pd.DataFrame({"n": [9]})
            self.log.append("NOTE: The data set WORK.RESULT has 1 observations.")
        else:
            self.listing += ["The SLC System", "", "  N", " 19"]
        return 0

    def getLog(self) -> Any:
        while self.log:
            yield self.log.pop(0)

    def getListingOutput(self) -> Any:
        while self.listing:
            yield self.listing.pop(0)

    def clearListingOutput(self) -> None:
        self.listing = []

    def get_library(self, name: str = "WORK") -> StandInLibrary:
        return StandInLibrary(self, name)

    def shutdown(self) -> None:
        pass


class NameValuePair:
    def __init__(self, name: str = "", value: str = "") -> None:
        self.name = name
        self.value = value


@pytest.fixture
def fake_slcpy(monkeypatch: pytest.MonkeyPatch) -> list[StandInSlc]:
    """Install slcPy modules whose SLC processes are stand-ins, and list them."""
    started: list[StandInSlc] = []

    def Slc(options: list[Any]) -> StandInSlc:
        slc = StandInSlc(options)
        started.append(slc)
        return slc

    slc_module = types.ModuleType("slc")
    slc_slc = types.ModuleType("slc.slc")
    slc_slc.Slc = Slc  # type: ignore[attr-defined]
    slc_library = types.ModuleType("slc.library")
    slc_library.OpenMode = types.SimpleNamespace(OpenModeRead=0)  # type: ignore[attr-defined]
    server = types.ModuleType("wpslink.wps.server")
    server.NameValuePair = NameValuePair  # type: ignore[attr-defined]
    for name, module in {
        "slc": slc_module,
        "slc.slc": slc_slc,
        "slc.library": slc_library,
        "wpslink": types.ModuleType("wpslink"),
        "wpslink.wps": types.ModuleType("wpslink.wps"),
        "wpslink.wps.server": server,
    }.items():
        monkeypatch.setitem(sys.modules, name, module)
    return started


STUDENTS = (
    "/**\n * Students of one sex\n * @measure\n * @param sex `enum[F, M]` Sex.\n */\n"
    'data work.result; set work.class; where sex = "&sex"; run;\n'
)


def test_an_slc_session_does_not_start_until_it_is_used() -> None:
    session = slc_session({"ENCODING": "UTF-8"})

    assert repr(session) == "<SlcSession (not started)>"
    assert session.sys_options == {"ENCODING": "UTF-8"}


def test_a_sas_measure_runs_on_slc(fake_slcpy: list[StandInSlc]) -> None:
    (spec,) = parse_sas_measures(STUDENTS, "students")
    session = slc_session({"ENCODING": "UTF-8"})

    frame = sas_measure(spec, session).func(sex="F")

    assert frame["n"].tolist() == [9]
    (slc,) = fake_slcpy
    assert [(o.name, o.value) for o in slc.options] == [("ENCODING", "UTF-8")]
    assert slc.submitted[0].startswith("data _null_;\n  call symputx('sex', 'F', 'G');")


def test_each_slc_submission_sees_only_its_own_log(
    fake_slcpy: list[StandInSlc],
) -> None:
    session = slc_session()

    first = session.submit("proc print data=work.class; run;")
    second = session.submit("data work.result; run;")

    assert first.listing == "The SLC System\n\n  N\n 19"
    assert "WORK.RESULT" not in first.log
    assert second.log.startswith("1    data work.result; run;")
    assert "proc print" not in second.log
    assert second.listing == ""


def test_run_sas_uploads_handles_to_slc(fake_slcpy: list[StandInSlc]) -> None:
    handles = HandleStore()
    handles.register(pd.DataFrame({"region": ["EMEA"]}))
    tool = run_sas_tool(slc_session(), handles)

    result = tool.func(code="proc print data=work.r1; run;")

    assert isinstance(result, ContentToolResult)
    assert result.value.startswith("Output:\nThe SLC System")
    (slc,) = fake_slcpy
    assert list(slc.tables) == ["WORK.R1"]
    assert slc.submitted[0] == XCMD_PROBE


def test_an_agent_runs_agent_code_in_an_slc_process_of_its_own(
    fake_slcpy: list[StandInSlc],
) -> None:
    trusted = slc_session()
    agent = Commons(
        scripted_chat(), data_source(sales=pd.DataFrame({"a": [1]})), sas=trusted
    )
    tool = next(tool for tool in agent.get_tools() if tool.name == "run_sas")

    assert fake_slcpy == []
    assert isinstance(tool, Tool)
    tool.func(code="proc print data=work.class; run;")

    assert len(fake_slcpy) == 1
    assert trusted._session is None
    assert isinstance(trusted.independent_copy(), SlcSession)


@pytest.mark.skipif(
    not os.environ.get("WPSHOME") and not os.path.isdir("/opt/altair/slc/2026"),
    reason="Altair SLC is not installed",
)
def test_a_sas_measure_runs_on_a_live_slc_process() -> None:
    pytest.importorskip("slc")
    session = slc_session()
    session.submit(
        "data work.class; input name $ sex $; datalines;\nAlice F\nBob M\nCarol F\n;\nrun;\n"
    )
    (spec,) = parse_sas_measures(STUDENTS, "students")

    frame = sas_measure(spec, session).func(sex="F")

    assert len(frame) == 2


@pytest.mark.skipif(
    not os.environ.get("WPSHOME") and not os.path.isdir("/opt/altair/slc/2026"),
    reason="Altair SLC is not installed",
)
def test_python_exits_after_using_a_live_slc_process() -> None:
    pytest.importorskip("slc")
    script = (
        "import commons\n"
        "session = commons.slc_session()\n"
        "session.submit('data work.result; x = 1; run;')\n"
        "assert session.table_exists('result')\n"
    )

    exited = subprocess.run(
        [sys.executable, "-c", script], capture_output=True, timeout=60, check=False
    )

    assert exited.returncode == 0, exited.stderr.decode()
