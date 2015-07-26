"""Utility classes needed to efficiently store location info.

Significant care is taken here to ensure minimal memory usage.

Absolutely nothing in this module is specific to the language being parsed.
"""

from typing import (
        Generic,
        IO,
        List,
        Optional,
        Tuple,
        TypeVar,
)

from abc import abstractmethod, ABCMeta
from bisect import bisect_right


T = TypeVar('T')


class EnsureSlotsMeta(type):
    """Metaclass to ensure that subclasses can't omit `__slots__`.

    There will be a *lot* of the objects during parsing ...
    """

    # Not annotated because mypy gets *really* confused with metaclasses.
    def __new__(mcls, name, bases, dct):
        slots = dct.setdefault('__slots__', ())
        if isinstance(slots, str):
            slots = (slots,)
            dct['__slots__'] = slots
        return type.__new__(mcls, name, bases, dct)

    def _all_slots(cls):
        rv = []
        for clz in cls.mro()[:-1]:
            rv.extend(clz.__slots__)
        return rv

class Span(tuple):
    """Source locations, packed as an immutable pair of integers.

    In order to support empty spans, `end` must be exclusive.

    See `CodeMap` for how to interpret this.
    """

    __slots__ = ()

    @property
    def begin(self) -> int:
        return self[0]

    @property
    def end(self) -> int:
        return self[1]

    def __new__(cls: type, begin: int, end: int, file: Optional['FileInfo']) -> 'Span':
        assert begin <= end
        if file is not None:
            begin += file.start_offset
            end += file.start_offset
        return tuple.__new__(Span, (begin, end)) # type: ignore

    @staticmethod
    def make(begin: 'Span', end: 'Span') -> 'Span':
        return Span(begin.begin, end.end, None)

    if False:
        # mypy doesn't support __new__ yet.
        def __init__(self, begin: int, end: int, file: 'FileInfo') -> None:
            ...

    def __repr__(self) -> str:
        return 'Span(%d, %d)' % (self.begin, self.end)

class HasSpan(metaclass=EnsureSlotsMeta):
    """Base class for everything that has a location for reporting errors."""

    __slots__ = ('_span')

    def __init__(self, span: Span) -> None:
        self._span = span

# TODO put blank Node/Tree/Token/BaseComment/EmptyToken/Eof classes here.


#
# Below this point, objects are either rare or short-lived.
#

class HumanLocation:
    """A human-readable location.

    These should *not* be long-lived!
    """

    __slots__ = ('filename', 'line', 'column', 'width', 'lines', 'column2')

    def __init__(self, filename: str, line: int, column: int, width: int, lines: List[str], column2: int) -> None:
        self.filename = filename
        self.line = line
        self.column = column
        self.width = width
        self.lines = lines
        self.column2 = column2

    def caret(self) -> str:
        width = self.width - 1
        column = self.column
        max_width = len(self.lines[0]) + 1 - column
        head = ' ' * (column - 1) + '^'
        tail = ''
        if width > max_width:
            width = max_width
            tail = ' ...'
        return head + ('~' * width) + tail

    def caret2(self) -> str:
        assert len(self.lines) > 1
        return '~' * (self.column2 + (not self.column2))

    def _as_tuple(self) -> Tuple[str, int, int, int, List[str], int]:
        return (
                self.filename,
                self.line,
                self.column,
                self.width,
                self.lines,
                self.column2,
        )

    def __repr__(self):
        return 'HumanLocation(%r, %r, %r, %r, %r, %r)' % self._as_tuple()

    def __eq__(self, other):
        return self._as_tuple() == other._as_tuple()

    def __ne__(self, other):
        return not (self == other)


# acceptable values for `level`.
DIAG_NOTE = 'note'
DIAG_WARNING = 'warning'
DIAG_ERROR = 'error'

class SingleDiagnostic:
    """A single diagnostic message, attached to a span.

    Not intended to be constructed directly.
    """

    __slots__ = ('location', 'level', 'message')

    def __init__(self, location: HumanLocation, level: str, message: str) -> None:
        self.location = location
        self.level = level
        self.message = message

    def format(self) -> str:
        file = self.location.filename
        line = self.location.line
        column = self.location.column
        level = self.level
        message = self.message
        return '%s:%s:%s: %s: %s' % (file, line, column, level, message)

class MultipleDiagnostics:
    """A sequence of related diagnostics messages.

    Not intended to be constructed directly.
    """

    __slots__ = ('pre', 'main', 'post', 'option')

    def __init__(self, pre: List[SingleDiagnostic], main: SingleDiagnostic, post: List[SingleDiagnostic], option: Optional[str]) -> None:
        # Typically, `pre` is used for "in this context"
        # and `post` is used for "look at this instead".
        self.pre = pre
        self.main = main
        self.post = post
        self.option = option


class ErrorStream(metaclass=ABCMeta):
    """Abstract base class for a sink for diagnostic messages.

    Despite the name, not all might be errors.
    """

    __slots__ = ('count',)

    def __init__(self):
        self.count = 0

    def emit(self, error: MultipleDiagnostics) -> None:
        if error.main.level == 'error':
            self.count += 1
        self._do_emit(error)

    @abstractmethod
    def _do_emit(self, error: MultipleDiagnostics) -> None:
        pass

class ForbidErrorStream(ErrorStream):
    """Implementation of an error stream that just throws an exception.

    This is useful in the testsuite for things that should not fail.
    """

    def _do_emit(self, error: MultipleDiagnostics) -> None:
        raise ValueError(error)

class FileErrorStream(ErrorStream):
    """Implementation of an error stream that writes to a file-like object.

    The main diagnostic is written with a copy of the line text and a caret.
    """
    # TODO color?

    __slots__ = ('stream',)

    def __init__(self, stream: IO[str]) -> None:
        super().__init__()
        self.stream = stream

    def _do_emit1(self, d: SingleDiagnostic, o: Optional[str]) -> None:
        msg = d.format()
        if o is not None:
            self.stream.writelines([msg, ' [', o, ']\n'])
        else:
            self.stream.writelines([msg, '\n'])
        msg = d.location.lines[0]
        caret = d.location.caret()
        self.stream.writelines([msg, '\n', caret, '\n'])
        if len(d.location.lines) != 1:
            if len(d.location.lines) != 2:
                self.stream.writelines('(%d lines omitted)\n' % (len(d.location.lines) - 2))
            msg = d.location.lines[-1]
            caret = d.location.caret2()
            self.stream.writelines([msg, '\n', caret, '\n'])

    def _do_emit(self, error: MultipleDiagnostics) -> None:
        for d in error.pre:
            self._do_emit1(d, None)
        self._do_emit1(error.main, error.option)
        for d in error.post:
            self._do_emit1(d, None)
        self.stream.flush()

class Bisector(Generic[T]):
    """Append-only implementation of range_map.

    Objects are appended with a size. Any index can then be looked up
    to show which object it's in, where in it, and the bounds of the object.
    """

    __slots__ = ('offsets', 'data')

    def __init__(self):
        self.offsets = [0]
        self.data = [] # type: List[T]

    def add(self, length: int, value: T) -> None:
        # TODO should this return start offset?
        assert length > 0
        self.offsets.append(self.offsets[-1] + length)
        self.data.append(value)

    def lookup(self, offset: int) -> Tuple[T, int, int, int]:
        if not (0 <= offset < self.offsets[-1]):
            raise KeyError(offset)
        index = bisect_right(self.offsets, offset)
        assert self.offsets[index - 1] <= offset < self.offsets[index]
        data = self.data[index - 1]
        soff = self.offsets[index - 1]
        eoff = self.offsets[index]
        # Technically this is redundant, but it keeps callers simpler.
        return (data, offset - soff, soff, eoff)

# TODO provide a way to drop the content, in which case `HumanLocation` will
# have `text=None`. Possibly even drop the line numbers?
class FileInfo:
    """Information about a single source file.

    Contains the name, entire contents, and line map.
    """

    __slots__ = ('name', 'content', 'lines', 'start_offset')

    def __init__(self, name: str, content: str, offset: int) -> None:
        self.start_offset = offset # type: int
        self.name = name
        self.content = content
        lines = self.lines = Bisector() # type: Bisector[int]
        assert content.endswith('\n')
        sol = 0
        eof = len(content)
        last_nl = len(content) - 1
        line_no = 1
        while sol != eof:
            next_sol = content.find('\n', sol) + 1
            delta = next_sol - sol
            lines.add(delta, line_no)
            sol = next_sol
            line_no += 1

    def lookup(self, offset: int) -> Tuple[int, int, int, int]:
        if offset == len(self.content):
            return (len(self.lines.offsets), 0, offset, offset)
        # returns `(line, raw column, sol, eol)`
        return self.lines.lookup(offset)

class CodeMap:
    """Keep track of all the sources.

    Typically there is only one of these at a time.
    """

    __slots__ = ('errors', 'files')

    def __init__(self, errors: Optional[ErrorStream] = None) -> None:
        if errors is None:
            import sys
            errors = FileErrorStream(sys.stderr)
        self.errors = errors
        self.files = Bisector() # type: Bisector[FileInfo]

    def add(self, name: str, content: str) -> FileInfo:
        # Canonicalize line endings to `'\n'` for simplicity.
        # Make sure that there is a `'\n'` at EOF (and forbid empty files).
        # Note that tabs are *not* canonicalized here.
        content = content.replace('\r\n', '\n').replace('\r', '\n')
        if not content.endswith('\n'):
            content += '\n'
        info = FileInfo(name, content, self.files.offsets[-1])
        self.files.add(len(content) + 1, info)
        return info

    def lookup(self, span: Span) -> HumanLocation:
        file, file_offset, sof, eof = self.files.lookup(span.begin)
        line, line_offset, sol, eol = file.lookup(file_offset)
        file_size = eof - sof
        line_size = eol - sol

        filename = file.name
        # TODO use the `wcwidth` module to correctly calculate the display
        # width of two-column (mostly Asian) and combining characters.
        column = line_offset + 1
        width = span.end - span.begin
        if file_offset + width >= file_size:
            # Note, this is the *adjusted* file size, i.e. including EOF.
            raise KeyError(span)
        # Negative slicing (at EOF only) is the same as empty in python.
        lines = [file.content[sol:eol-1]]

        li = line - 1
        more = width + line_offset - line_size
        while more > 0:
            li += 1
            sol = eol
            eol = file.lines.offsets[li + 1]
            line_size = eol - sol
            lines.append(file.content[sol:eol-1])
            more -= line_size
        more += line_size
        return HumanLocation(filename, line, column, width, lines, more)

    def emit_multiple(self, diags: MultipleDiagnostics) -> None:
        self.errors.emit(diags)

    def emit_one(self, diag: SingleDiagnostic) -> None:
        self.emit_multiple(MultipleDiagnostics([], diag, [], None))

    # The below convenience functions may change as error-reporting grows.
    # In particular, they should probably take advantage of `HasSpan`.

    def diag(self, location: Span, level: str, message: str, option: str = None, pre: List[Tuple[Span, str]] = [], post: List[Tuple[Span, str]] = []) -> None:
        pres = [SingleDiagnostic(self.lookup(sp), 'note', m) for (sp, m) in pre]
        main = SingleDiagnostic(self.lookup(location), level, message)
        posts = [SingleDiagnostic(self.lookup(sp), 'note', m) for (sp, m) in post]
        self.emit_multiple(MultipleDiagnostics(pres, main, posts, option))

    def error(self, location: Span, message: str, option: str = None, pre: List[Tuple[Span, str]] = [], post: List[Tuple[Span, str]] = []) -> None:
        self.diag(location, 'error', message, option, pre, post)

    def warning(self, location: Span, message: str, option: str = None, pre: List[Tuple[Span, str]] = [], post: List[Tuple[Span, str]] = []) -> None:
        self.diag(location, 'warning', message, option, pre, post)

# Planned: an option manager that automatically calls error, warning,
# or nothing, as appropriate for the flags passed on the command-line.
# Flags may be -Wno-foo, -Wfoo, or -Werror=foo. Each flag has a default,
# which may be an expression, e.g. default -Wfoo = (-Wextra or -Wunused).
# Doing it this way is important so that -Wno-foo -Wextra DTRT.
# Note that there will always be errors (but not warnings) that are not
# controlled by any option.
