"""Trusted SAS code: measures read from .sas files and run through SASPy.

A .sas file declares its measures in `/** ... */` header blocks tagged
``@measure``. Each becomes an ordinary :class:`Measure` whose function binds
the call's arguments as SAS macro variables, submits the measure's code, and
returns its output dataset as a data frame, so ``call_measure`` runs it like
any other trusted calculation.

The header grammar, the macro binding, and the log check are a cross-language
contract pinned by ``tests/shared/sas-measures.json``; ``pkg-r/R/sas.R``
implements the same contract for R.
"""

from __future__ import annotations

import re
import threading
from collections.abc import Mapping, Sequence
from dataclasses import dataclass, field, replace
from pathlib import Path
from typing import Any, Literal, Protocol, cast

from pydantic import ConfigDict, Field, create_model

from ._measures import SOURCE_TEXT_ATTRIBUTE, Measure


class SasMeasureError(ValueError):
    """A .sas file's measure header cannot be read.

    ``code`` is the slug ``tests/shared/sas-measures.json`` pins; the message
    is for people.
    """

    def __init__(self, code: str, message: str) -> None:
        super().__init__(message)
        self.code = code


class SasRunError(RuntimeError):
    """A SAS measure ran, but SAS reported an error or produced no output."""


# --- Sessions ---------------------------------------------------------------


@dataclass(frozen=True)
class SasResult:
    log: str
    listing: str


class _SaspyLike(Protocol):
    def submit(self, code: str, results: str = ...) -> Mapping[str, str]: ...
    def exist(self, table: str, libref: str = ...) -> bool: ...
    def sd2df(self, table: str, libref: str = ...) -> Any: ...
    def df2sd(self, df: Any, table: str = ..., libref: str = ...) -> Any: ...


class SasSession:
    """A connection to SAS, opened through SASPy on first use.

    Constructing one does not connect, so a semantic layer of SAS measures can
    be built before SAS is reachable. ``cfgname`` names a SASPy configuration
    (see SASPy's ``sascfg_personal.py``); ``None`` uses SASPy's default.
    Pass ``session`` to reuse a ``saspy.SASsession`` you already opened.

    One SAS session runs one submission at a time, so calls are serialized.
    """

    def __init__(
        self,
        cfgname: str | None = None,
        *,
        session: Any = None,
        **saspy_options: Any,
    ) -> None:
        self.cfgname = cfgname
        self._saspy_options = saspy_options
        self._session: _SaspyLike | None = session
        self._lock = threading.RLock()

    def __repr__(self) -> str:
        state = "connected" if self._session is not None else "not connected"
        config = f" {self.cfgname!r}" if self.cfgname else ""
        return f"<SasSession{config} ({state})>"

    def _connect(self) -> _SaspyLike:
        if self._session is None:
            try:
                import saspy  # pyrefly: ignore[missing-import]
            except ImportError as error:
                raise ImportError(
                    "Running SAS measures requires SASPy.\n"
                    "Install it with `pip install commons[sas]`, then configure "
                    "a connection: https://sassoftware.github.io/saspy/configuration.html"
                ) from error
            options = dict(self._saspy_options)
            if self.cfgname is not None:
                options["cfgname"] = self.cfgname
            self._session = cast(_SaspyLike, saspy.SASsession(**options))
        return self._session

    def submit(self, code: str) -> SasResult:
        with self._lock:
            result = self._connect().submit(code, results="TEXT")
        return SasResult(
            log=str(result.get("LOG", "")), listing=str(result.get("LST", ""))
        )

    def table_exists(self, table: str, libref: str = "WORK") -> bool:
        with self._lock:
            return bool(self._connect().exist(table, libref))

    def to_frame(self, table: str, libref: str = "WORK") -> Any:
        with self._lock:
            return self._connect().sd2df(table, libref)

    def from_frame(self, frame: Any, table: str, libref: str = "WORK") -> None:
        with self._lock:
            self._connect().df2sd(frame, table=table, libref=libref)


def sas_session(cfgname: str | None = None, **saspy_options: Any) -> SasSession:
    """Describe a SAS connection for SAS measures; it opens on first use."""
    return SasSession(cfgname, **saspy_options)


_default_session: SasSession | None = None
_default_lock = threading.Lock()


def _default_sas_session() -> SasSession:
    global _default_session
    with _default_lock:
        if _default_session is None:
            _default_session = SasSession()
        return _default_session


# --- Header parsing ---------------------------------------------------------


_SCALAR_KINDS = ("string", "integer", "number", "boolean")
_IDENTIFIER = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*$")
_MACRO_NAME = re.compile(r"^[A-Za-z_][A-Za-z0-9_]{0,31}$")
_LIBREF = re.compile(r"^[A-Za-z_][A-Za-z0-9_]{0,7}$")
_BLOCK = re.compile(r"(?m)^[ \t]*/\*\*(.*?)\*/", re.DOTALL)
_TAG = re.compile(r"^@(\w+)\s*(.*)$")
_TYPE_CODE = re.compile(r"^`([^`]*)`\s*(.*)$", re.DOTALL)
_ENUM = re.compile(r"^enum\[([^\]]*)\]$")
_PARAM = re.compile(r"^(\[[^\]]*\]|\S+)\s*(.*)$", re.DOTALL)
_KNOWN_TAGS = frozenset({"measure", "param", "return", "output", "provenance"})


@dataclass(frozen=True)
class SasArgument:
    name: str
    type: str
    description: str = ""
    required: bool = True
    values: tuple[str, ...] = ()
    items: SasArgument | None = None
    default: Any = None


@dataclass(frozen=True)
class SasMeasureSpec:
    name: str
    title: str
    description: str
    arguments: tuple[SasArgument, ...]
    output: tuple[str, str] | None
    provenance: tuple[str, ...]
    code: str
    file: str | None = field(default=None, compare=False)


def parse_sas_measures(text: str, stem: str) -> list[SasMeasureSpec]:
    """Read every measure block in one .sas file's text.

    ``stem`` is the file name without its extension, the name of a measure
    whose ``@measure`` tag names none.
    """
    text = text.replace("\r\n", "\n")
    blocks = [
        match for match in _BLOCK.finditer(text) if _is_measure_block(match.group(1))
    ]
    if not blocks:
        return []

    preamble = _trim_code(text[: blocks[0].start()])
    specs: list[SasMeasureSpec] = []
    for index, block in enumerate(blocks):
        end = blocks[index + 1].start() if index + 1 < len(blocks) else len(text)
        code = _trim_code(text[block.end() : end])
        if preamble:
            code = f"{preamble}\n\n{code}"
        spec = _parse_block(block.group(1), stem, code)
        if any(seen.name == spec.name for seen in specs):
            raise SasMeasureError(
                "duplicate-name",
                f"Two measures in {stem}.sas are named {spec.name!r}.\n"
                f"Name each with `@measure <name>`.",
            )
        specs.append(spec)
    return specs


def _block_lines(body: str) -> list[str]:
    lines = []
    for line in body.split("\n"):
        lines.append(line.lstrip().removeprefix("*").strip())
    return lines


def _is_measure_block(body: str) -> bool:
    return any(
        (match := _TAG.match(line)) is not None and match.group(1) == "measure"
        for line in _block_lines(body)
    )


def _trim_code(code: str) -> str:
    return re.sub(r"^(?:[ \t]*\n)+", "", code).rstrip()


def _parse_block(body: str, stem: str, code: str) -> SasMeasureSpec:
    paragraphs: list[list[str]] = [[]]
    tags: list[tuple[str, list[str]]] = []
    for line in _block_lines(body):
        tag = _TAG.match(line)
        if tag is not None:
            tags.append((tag.group(1), [tag.group(2)] if tag.group(2) else []))
        elif not line:
            if tags:
                tags.append(("", []))  # a blank line ends a tag's text
            elif paragraphs[-1]:
                paragraphs.append([])
        elif tags:
            tags[-1][1].append(line)
        else:
            paragraphs[-1].append(line)

    prose = [" ".join(paragraph) for paragraph in paragraphs if paragraph]
    name: str | None = None
    arguments: list[SasArgument] = []
    returns: str | None = None
    output: tuple[str, str] | None = ("WORK", "RESULT")
    provenance: list[str] = []

    for tag, parts in tags:
        value = " ".join(parts).strip()
        if tag == "":
            continue
        if tag not in _KNOWN_TAGS:
            raise SasMeasureError(
                "unknown-tag",
                f"Unknown tag @{tag} in {stem}.sas.\n"
                f"Measure blocks use @measure, @param, @return, @output, and @provenance.",
            )
        if tag == "measure":
            name = value or None
        elif tag == "param":
            argument = _parse_param(value, stem)
            if any(seen.name.lower() == argument.name.lower() for seen in arguments):
                raise SasMeasureError(
                    "duplicate-param",
                    f"Argument {argument.name!r} is declared twice in {stem}.sas.",
                )
            arguments.append(argument)
        elif tag == "return":
            returns = value
        elif tag == "output":
            output = _parse_output(value, stem)
        elif tag == "provenance":
            provenance.append(value)

    resolved = name or stem
    if not _IDENTIFIER.match(resolved):
        raise SasMeasureError(
            "invalid-name",
            f"{resolved!r} is not a valid measure name.\n"
            f"Use letters, digits, and underscores, or name it with `@measure <name>`.",
        )
    if not prose:
        raise SasMeasureError(
            "no-description",
            f"Measure {resolved!r} in {stem}.sas has no title.\n"
            f"Start its header block with a line saying what it computes.",
        )

    description = "\n\n".join([*prose, *([f"Returns: {returns}"] if returns else [])])
    return SasMeasureSpec(
        name=resolved,
        title=prose[0],
        description=description,
        arguments=tuple(arguments),
        output=output,
        provenance=tuple(provenance),
        code=code,
    )


def _parse_param(text: str, stem: str) -> SasArgument:
    match = _PARAM.match(text)
    if match is None:
        raise SasMeasureError("invalid-param", f"Empty @param in {stem}.sas.")
    head, rest = match.groups()

    required = True
    default_text: str | None = None
    if head.startswith("["):
        required = False
        head = head[1:-1].strip()
        if "=" in head:
            head, default_text = (part.strip() for part in head.split("=", 1))
    if not _MACRO_NAME.match(head):
        raise SasMeasureError(
            "invalid-param",
            f"{head!r} in {stem}.sas is not a SAS macro variable name.",
        )

    type_code = "string"
    typed = _TYPE_CODE.match(rest)
    if typed is not None:
        type_code, rest = typed.group(1).strip(), typed.group(2)
    argument = _parse_type(type_code, head, rest.strip(), required, stem)

    if default_text is not None:
        default = _parse_default(argument, default_text)
        if default is None:
            raise SasMeasureError(
                "invalid-default",
                f"Default {default_text!r} for {head!r} in {stem}.sas does not fit its type.",
            )
        argument = replace(argument, default=default)
    return argument


def _parse_type(
    code: str, name: str, description: str, required: bool, stem: str
) -> SasArgument:
    if code.endswith("[]"):
        items = _parse_type(code[:-2].strip(), name, "", True, stem)
        if items.type == "array":
            raise SasMeasureError(
                "invalid-param", f"Nested arrays for {name!r} in {stem}.sas."
            )
        return SasArgument(name, "array", description, required, items=items)
    enum = _ENUM.match(code)
    if enum is not None:
        values = tuple(value.strip() for value in enum.group(1).split(","))
        return SasArgument(name, "enum", description, required, values=values)
    if code in _SCALAR_KINDS:
        return SasArgument(name, code, description, required)
    raise SasMeasureError(
        "invalid-param",
        f"Unknown type `{code}` for {name!r} in {stem}.sas.\n"
        f"Use string, integer, number, boolean, enum[...], or an array of one of them.",
    )


def _parse_default(argument: SasArgument, text: str) -> Any:
    kind = argument.type
    if kind == "string":
        return text
    if kind == "enum":
        return text if text in argument.values else None
    if kind == "integer":
        return int(text) if re.fullmatch(r"[+-]?\d+", text) else None
    if kind == "number":
        try:
            return float(text)
        except ValueError:
            return None
    if kind == "boolean":
        return {"true": True, "false": False, "1": True, "0": False}.get(text.lower())
    return None  # arrays take no default


def _parse_output(text: str, stem: str) -> tuple[str, str] | None:
    if text.lower() == "none":
        return None
    libref, _, table = text.rpartition(".")
    libref = libref or "WORK"
    if not (_LIBREF.match(libref) and _MACRO_NAME.match(table)):
        raise SasMeasureError(
            "invalid-output",
            f"@output {text!r} in {stem}.sas is not a SAS dataset name.\n"
            f"Use LIBREF.TABLE, TABLE (in WORK), or none.",
        )
    return libref.upper(), table.upper()


# --- Running ----------------------------------------------------------------


def _sas_literal(value: str) -> str:
    return "'" + value.replace("'", "''") + "'"


def _macro_text(argument: SasArgument, value: Any) -> str:
    if value is None:
        return ""
    kind = argument.type
    if kind == "array":
        assert argument.items is not None
        items = argument.items
        if items.type in ("string", "enum"):
            return " ".join(_sas_literal(str(item)) for item in value)
        return " ".join(_macro_text(items, item) for item in value)
    if kind == "boolean":
        return "1" if value else "0"
    if kind == "integer":
        return str(int(value))
    if kind == "number":
        return f"{float(value):.15g}"
    return str(value)


def sas_prelude(
    arguments: Sequence[SasArgument],
    output: tuple[str, str] | None,
    values: Mapping[str, Any],
) -> str:
    """The SAS submitted ahead of a measure's code for one call."""
    lines: list[str] = []
    if arguments:
        lines.append("data _null_;")
        for argument in arguments:
            text = _macro_text(argument, values.get(argument.name))
            lines.append(
                f"  call symputx({_sas_literal(argument.name)}, {_sas_literal(text)}, 'G');"
            )
        lines.append("run;")
    if output is not None:
        libref, table = output
        lines += [
            f"proc datasets lib={libref} nolist nowarn;",
            f"  delete {table};",
            "quit;",
        ]
    return "".join(f"{line}\n" for line in lines)


_LOG_ERROR = re.compile(r"^ERROR(?:\s+\d+-\d+)?:")


def sas_log_errors(log: str) -> list[str]:
    """The error lines of a SAS log; empty when the run succeeded."""
    return [line.rstrip() for line in log.splitlines() if _LOG_ERROR.match(line)]


def _run(spec: SasMeasureSpec, session: SasSession, values: Mapping[str, Any]) -> Any:
    result = session.submit(
        sas_prelude(spec.arguments, spec.output, values) + spec.code
    )
    errors = sas_log_errors(result.log)
    if errors:
        shown = "\n".join(errors[:5])
        raise SasRunError(
            f"SAS reported errors running measure {spec.name!r}:\n{shown}"
        )
    if spec.output is None:
        return result.listing
    libref, table = spec.output
    if not session.table_exists(table, libref):
        raise SasRunError(
            f"Measure {spec.name!r} ran without creating {libref}.{table}.\n"
            f"Its code must create the table named by @output."
        )
    return session.to_frame(table, libref)


# --- Measures ---------------------------------------------------------------


_ANNOTATIONS: dict[str, Any] = {
    "string": str,
    "integer": int,
    "number": float,
    "boolean": bool,
}


def _annotation(argument: SasArgument) -> Any:
    if argument.type == "enum":
        return Literal[argument.values]  # type: ignore[valid-type]
    if argument.type == "array":
        assert argument.items is not None
        return list[_annotation(argument.items)]
    return _ANNOTATIONS[argument.type]


def sas_measure(spec: SasMeasureSpec, session: SasSession | None = None) -> Measure:
    """Build the measure that runs ``spec`` on ``session``.

    Without a session, the measure runs on a process-wide default one, opened
    with SASPy's default configuration when it is first needed.
    """
    fields: dict[str, Any] = {}
    for argument in spec.arguments:
        annotation = _annotation(argument)
        if argument.required:
            fields[argument.name] = (
                annotation,
                Field(description=argument.description),
            )
        elif argument.default is not None:
            fields[argument.name] = (
                annotation,
                Field(default=argument.default, description=argument.description),
            )
        else:
            fields[argument.name] = (
                annotation | None,
                Field(default=None, description=argument.description),
            )

    def run(**values: Any) -> Any:
        bound = {
            argument.name: values.get(argument.name, argument.default)
            for argument in spec.arguments
        }
        return _run(spec, session or _default_sas_session(), bound)

    run.__name__ = spec.name
    run.__qualname__ = spec.name
    run.__doc__ = spec.description
    # What the agent's worker session shows as this measure's definition: the
    # SAS it runs, not this wrapper.
    setattr(run, SOURCE_TEXT_ATTRIBUTE, spec.code)

    return Measure(
        name=spec.name,
        title=spec.title,
        description=spec.description,
        func=run,
        params=create_model(spec.name, __config__=ConfigDict(extra="forbid"), **fields),
        provenance=spec.provenance,
    )


def read_sas_measures(
    path: Path, session: SasSession | None = None
) -> tuple[list[Measure], dict[str, str]]:
    """Read one .sas file into measures and the SAS source of each."""
    specs = parse_sas_measures(path.read_text(encoding="utf-8"), path.stem)
    return [sas_measure(spec, session) for spec in specs], {
        spec.name: spec.code for spec in specs
    }


def sas_measures(
    *paths: str | Path, session: SasSession | None = None
) -> list[Measure]:
    """Read SAS measures from .sas files or directories of them.

    Pass the result to :func:`semantic_layer`. ``session`` is the SAS
    connection the measures run on; a .sas path given to
    :func:`semantic_layer` directly runs on a default session instead.
    """
    measures: list[Measure] = []
    for path in map(Path, paths):
        if path.is_dir():
            files = sorted(
                entry for entry in path.iterdir() if entry.suffix.lower() == ".sas"
            )
        elif path.suffix.lower() == ".sas":
            files = [path]
        else:
            raise ValueError(f"{path} is not a .sas file or a directory.")
        for file in files:
            measures.extend(read_sas_measures(file, session)[0])
    return measures
