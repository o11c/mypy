"""List of tokens.

Any given Python version only supports a subset of these tokens.
"""


from typing import (
        List as _List,
)

from abc import (
        abstractmethod as _abstractmethod,
)

from mypy.syntax.span import (
        HasSpan as _HasSpan,
        Span as _Span,
)


class Comment(_HasSpan):
    __slots__ = ('_whitespace', '_text')

    def __init__(self, whitespace: str, blackspace: str, span: _Span) -> None:
        super().__init__(span)
        self._whitespace = whitespace
        self._text = blackspace

    def __repr__(self) -> str:
        return '%s(%r, %r, %r)' % (self.__class__.__name__, self._whitespace, self._text, self._span)

    def __eq__(self, other) -> bool:
        return (
                type(self) is type(other)
                and self._span == other._span
                and self._whitespace == other._whitespace
                and self._text == other._text
        )

    def __ne__(self, other) -> bool:
        return not (self == other)

class HasToken(_HasSpan):

    @_abstractmethod
    def head(self) -> 'Token':
        ...

    @_abstractmethod
    def tail(self) -> 'Token':
        ...

class Token(HasToken):
    """Base class for full tokens, with comments embedded.
    """

    __slots__ = ('_whitespace', '_text', '_comments')

    def __init__(self, comments: _List[Comment], whitespace: str, blackspace: str, span: _Span) -> None:
        super().__init__(span)
        self._whitespace = whitespace
        self._text = blackspace
        self._comments = comments

    def __repr__(self) -> str:
        return '%s(%r, %r, %r, %r)' % (self.__class__.__name__, self._comments, self._whitespace, self._text, self._span)

    def __eq__(self, other) -> bool:
        return (
                type(self) is type(other)
                and self._span == other._span
                and self._whitespace == other._whitespace
                and self._text == other._text
                and self._comments == other._comments
        )

    def __ne__(self, other) -> bool:
        return not (self == other)

    def head(self) -> 'Token':
        return self

    def tail(self) -> 'Token':
        return self

class TokError(Token): pass

class TokNewline(Token): pass
class TokIndent(Token): pass
class TokDedent(Token): pass
class TokEof(Token): pass

class OpBacktick(Token): pass
class OpTilde(Token): pass
class OpLess(Token): pass
class OpGreater(Token): pass
class OpLessEqual(Token): pass
class OpGreaterEqual(Token): pass
class OpEqEqual(Token): pass
class OpNotEqual(Token): pass

class OpPlus(Token): pass
class OpMinus(Token): pass
class OpStar(Token): pass
class OpStarStar(Token): pass
class OpClassicSlash(Token): pass
class OpSlash(Token): pass
class OpSlashSlash(Token): pass
class OpPercent(Token): pass
class OpAt(Token): pass
class OpLShift(Token): pass
class OpRShift(Token): pass
class OpAmper(Token): pass
class OpVBar(Token): pass
class OpCircumflex(Token): pass

class OpPlusEqual(Token): pass
class OpMinusEqual(Token): pass
class OpStarEqual(Token): pass
class OpStarStarEqual(Token): pass
class OpClassicSlashEqual(Token): pass
class OpSlashEqual(Token): pass
class OpSlashSlashEqual(Token): pass
class OpPercentEqual(Token): pass
class OpAtEqual(Token): pass
class OpLShiftEqual(Token): pass
class OpRShiftEqual(Token): pass
class OpAmperEqual(Token): pass
class OpVBarEqual(Token): pass
class OpCircumflexEqual(Token): pass

class OpEqual(Token): pass
class OpLPar(Token): pass
class OpRPar(Token): pass
class OpLSqb(Token): pass
class OpRSqb(Token): pass
class OpLBrace(Token): pass
class OpRBrace(Token): pass
class OpComma(Token): pass
class OpColon(Token): pass
class OpDot(Token): pass
class OpDotDotDot(Token): pass
class OpSemicolon(Token): pass
class OpRArrow(Token): pass

class LitBytes(Token): pass
class LitUnicode(Token): pass
class LitInteger(Token): pass
class LitFloat(Token): pass
class LitImaginary(Token): pass

class TokName(Token): pass

class KwDebug(Token): pass
class KwFalse(Token): pass
class KwNone(Token): pass
class KwTrue(Token): pass

class KwAnd(Token): pass
class KwIn(Token): pass
class KwIs(Token): pass
class KwNot(Token): pass
class KwOr(Token): pass

class KwAsync(Token): pass
class KwAs(Token): pass
class KwAssert(Token): pass
class KwAwait(Token): pass
class KwBreak(Token): pass
class KwClass(Token): pass
class KwContinue(Token): pass
class KwDef(Token): pass
class KwDel(Token): pass
class KwElif(Token): pass
class KwElse(Token): pass
class KwExcept(Token): pass
class KwExec(Token): pass
class KwFinally(Token): pass
class KwFor(Token): pass
class KwFrom(Token): pass
class KwGlobal(Token): pass
class KwIf(Token): pass
class KwImport(Token): pass
class KwLambda(Token): pass
class KwNonlocal(Token): pass
class KwPass(Token): pass
class KwPrint(Token): pass
class KwRaise(Token): pass
class KwReturn(Token): pass
class KwTry(Token): pass
class KwWhile(Token): pass
class KwWith(Token): pass
class KwYield(Token): pass
