"""Agent-written SAS: the run_sas tool.

SAS code runs on the SAS server, beyond the reach of the sandbox that contains
agent-written code elsewhere. So run_sas runs agent code only in a session
whose server was started with NOXCMD, which shuts off host commands, and only
in a session of its own: trusted SAS measures never share one with agent code,
which could otherwise change options, librefs, or macros they rely on.

What a submission tells the model, and the probe that checks NOXCMD, are a
cross-language contract pinned by ``tests/shared/run-sas.json``;
``pkg-r/R/run-sas.R`` implements it for R. The tool's description is
``prompts/run-sas-tool.md``, which both packages ship.
"""

from __future__ import annotations

import re
import threading

from chatlas import ContentToolResult, Tool
from htmltools import Tag, div, tags

from ._citations import CitationRequest, tool_result
from ._display import ANALYSIS
from ._frames import is_frame
from ._handles import HandleStore
from ._prompt import read_prompt
from ._provenance import Tag as ProvenanceTag
from ._sas import SasResult, SasSession

MAX_LISTING_CHARS = 10_000
MAX_LOG_CHARS = 6_000
XCMD_PROBE = "%put COMMONS_XCMD=%sysfunc(getoption(XCMD));\n"

_MESSAGE = re.compile(r"^(?:ERROR|WARNING|NOTE)(?:\s+\d+-\d+)?:")
_PROCESS_TIME = "used (Total process time):"
_XCMD_SETTING = re.compile(r"^COMMONS_XCMD=(\w+)\s*$")


class SasLockdownError(RuntimeError):
    """The SAS session allows host commands, so agent code may not run in it."""


def sas_log_messages(log: str) -> list[str]:
    """The ERROR, WARNING, and NOTE messages of a SAS log, one string each."""
    messages: list[list[str]] = []
    current: list[str] | None = None
    for raw in log.splitlines():
        line = raw.rstrip()
        if _MESSAGE.match(line):
            current = [line]
            messages.append(current)
        elif current is not None and line[:1] in (" ", "\t") and line.strip():
            current.append(line)
        else:
            current = None
    return [
        "\n".join(message)
        for message in messages
        if not (message[0].startswith("NOTE") and message[0].endswith(_PROCESS_TIME))
    ]


def sas_listing_text(listing: str) -> str:
    lines = [line.rstrip() for line in listing.replace("\f", "").splitlines()]
    return "\n".join(lines).lstrip("\n").rstrip()


def _cap(text: str, limit: int, what: str) -> str:
    if len(text) <= limit:
        return text
    return f"{text[:limit]}\n[{what} truncated.]"


def run_sas_value(log: str, listing: str) -> str:
    """What the model is told one submission produced."""
    messages = sas_log_messages(log)
    text = sas_listing_text(listing)
    parts: list[str] = []
    if any(message.startswith("ERROR") for message in messages):
        parts.append("SAS reported errors. Fix the code and run it again.")
    if text:
        parts.append("Output:\n" + _cap(text, MAX_LISTING_CHARS, "Output"))
    if messages:
        parts.append("Log:\n" + _cap("\n".join(messages), MAX_LOG_CHARS, "Log"))
    return "\n\n".join(parts) or "(The code ran but produced no output.)"


def xcmd_setting(log: str) -> str | None:
    """The XCMD setting the probe printed, or None if it printed none."""
    setting = None
    for line in log.splitlines():
        match = _XCMD_SETTING.match(line)
        if match is not None:
            setting = match.group(1).upper()
    return setting


def run_sas_html(code: str, log: str, listing: str) -> Tag:
    output = "\n\n".join(
        part
        for part in (sas_listing_text(listing), "\n".join(sas_log_messages(log)))
        if part
    )
    blocks = [
        tags.pre(tags.code(code, class_="language-sas"), class_="commons-run-r-code")
    ]
    if output:
        blocks.append(tags.pre(tags.code(output), class_="commons-run-r-code"))
    return div(*blocks, class_="commons-run-r-display")


class AgentSas:
    """The agent's own SAS session, checked once and kept in step with handles.

    Each data-frame result in the handle store is uploaded once, as the WORK
    dataset named after its handle, before the first submission after it.
    """

    def __init__(self, session: SasSession, handles: HandleStore) -> None:
        self._session = session
        self._handles = handles
        self._uploaded: set[str] = set()
        self._checked = False
        self._lock = threading.Lock()

    def submit(self, code: str) -> SasResult:
        with self._lock:
            self._check()
            for handle in self._handles.ids():
                if handle in self._uploaded:
                    continue
                value = self._handles.get(handle)
                if is_frame(value):
                    frame = value.to_pandas() if hasattr(value, "to_pandas") else value
                    self._session.from_frame(frame, handle)
                self._uploaded.add(handle)
            return self._session.submit(code)

    def _check(self) -> None:
        if self._checked:
            return
        setting = xcmd_setting(self._session.submit(XCMD_PROBE).log)
        if setting != "NOXCMD":
            found = f"reports {setting}" if setting else "did not report its setting"
            raise SasLockdownError(
                f"run_sas is unavailable: the SAS session {found}, and commons "
                "runs agent-written SAS only on a server started with NOXCMD. "
                "Tell the user that analysis in SAS is not available."
            )
        self._checked = True


def run_sas_tool(
    session: SasSession,
    handles: HandleStore,
    citation_request: CitationRequest | None = None,
) -> Tool:
    """The run_sas tool, running agent code in ``session``."""
    agent_sas = AgentSas(session, handles)

    def run_sas(code: str) -> ContentToolResult:
        result = agent_sas.submit(code)
        content = tool_result(
            run_sas_value(result.log, result.listing),
            tag=ProvenanceTag.B,
            title=ANALYSIS.settled,
            html=run_sas_html(code, result.log, result.listing),
        )
        if citation_request is None:
            return content
        return citation_request.add_request(content)

    return Tool(
        func=run_sas,
        name="run_sas",
        description=read_prompt("run-sas-tool.md"),
        parameters={
            "type": "object",
            "properties": {
                "code": {"type": "string", "description": "The SAS code to run."}
            },
            "required": ["code"],
            "additionalProperties": False,
        },
        annotations={
            "title": ANALYSIS.running,
            "readOnlyHint": False,
            "openWorldHint": True,
        },
    )
