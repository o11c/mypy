"""Parser for mypy.

Constructs a parse tree from a source file. May support features that mypy
and/or a given release of python does not.

Does not perform any semantic checks, those are only performed after
the parse tree is transformed into an AST (see classes in mypy.nodes).

Invariant: *every* token pulled will go in the parse tree somewhere. However,
there is no guarantee that any given subtree's span will cover any particular
subset of its children's spans.
"""

from typing import (
        Any,
        Callable,
        Dict,
        Generic,
        Iterable,
        List,
        Optional,
        Set,
        Tuple,
        TypeVar,
        Union,
        cast,
)

import codecs
import gc
import re

from mypy.syntax.dialect import Dialect
from mypy.syntax.lexer import Lexer, TokenFile, TokenBlock, TokenLine, TokenTree
from mypy.syntax.span import (
        CodeMap,
        FileInfo,
        HasSpan,
)
# `mypy.syntax.tree` reexports all of `mypy.syntax.tokens`.
#from mypy.syntax.tokens import *
from mypy.syntax.tree import *


MaybeTokenBlock = Union[Token, TokenLine, TokenBlock]
MaybeTokenTree = Union[Token, TokenTree]

T = TypeVar('T')
U = TypeVar('U')


_comments_regex = re.compile(b'(?:[ \t\f\n]|#.*)*')
_coding_regex = re.compile(b'(?<=coding[:=])[ \t\f]*[-\w.]+')


def infer_encoding(txt: bytes, default: str) -> str:
    has_bom = txt.startswith(codecs.BOM_UTF8)
    nl1 = txt.find(b'\n') + 1 or len(txt)
    nl2 = txt.find(b'\n', nl1) + 1 or len(txt)
    if has_bom:
        txt = txt[len(codecs.BOM_UTF8):nl2]
    else:
        txt = txt[0:nl2]
    txt = _comments_regex.match(txt).group()
    codings = _coding_regex.findall(txt)
    if len(codings) > 1:
        raise ValueError('multiple `coding:` comments')
    if codings:
        coding = codings[0].decode('ascii').lstrip()
    else:
        coding = default
    if has_bom:
        if codings and coding != 'utf-8':
            raise ValueError('`coding: %s` conflicts with BOM' % coding)
        return 'utf-8-sig'
    return coding


_del_assert_messages = [] # type: List[str]

def _check_assert_messages() -> None:
    # Be VERY careful that you don't call this from a place where you have
    # another peekable open - that will lead to cascading errors.
    l = len(_del_assert_messages)
    if l:
        # When the GC is called is unspecified, so messages may be added
        # at any time. But this function is the only thing that *shrinks*
        # the array, so this is safe.
        m = _del_assert_messages[:l]
        del _del_assert_messages[:l]
        raise AssertionError('\n'.join(m))


class BasePeekable:
    # I considered refactoring out a generic base class, but too much of
    # this class's logic is specific to HasToken.

    # TODO use `list` instead of `iter` ?
    def __init__(self, it: Iterable[HasToken], sentinel: HasToken, parent: Optional['BasePeekable']) -> None:
        self.it = iter(it)
        self.sentinel = sentinel
        self.cache = None # type: HasToken
        self.parent = parent
        self.expected = parent.expected if parent else set() # type: Set[str]

    def __del__(self, messages: List[str] = _del_assert_messages) -> None:
        if self.it is not None:
            messages.append('Broken code! Lost %r + %r' % (list(self.it), self.sentinel))

    # Basic API.

    def peek(self) -> HasToken:
        try:
            _check_assert_messages()
        except:
            # prevent cascading error
            self.it = None
            raise
        if self.cache is None:
            if self.it is None:
                assert False, 'Broken code! Trying to reuse sentinel %r' % self.sentinel
            self.cache = next(self.it, self.sentinel)
        return self.cache

    def pop(self) -> HasToken:
        # self.cache is *usually* filled, not always for top-level callers.
        rv = self.peek()
        if self.cache is self.sentinel:
            self.it = None
        else:
            assert type(self.cache) is not type(self.sentinel), 'nonsentinel %s' % type(self.sentinel).__name__
        self.cache = None
        self.expected.clear()
        return rv

    # Extended API.

    def expect(self, t: Callable[..., U]) -> U:
        # Use this ONLY if already forced by previous check.
        rv = self.pop()
        assert isinstance(rv, cast(type, t)), '%s != %s' % (type(rv).__name__, t.__name__)
        return cast(U, rv)

    def ensure(self, t: Callable[..., U], cm: CodeMap) -> U:
        rv = self.pop_if(t)
        if rv is not None:
            return rv
        # This is not always the *best* error recovery strategy,
        # but it is no worse than waiting till end of line or parens,
        # and often better.
        return cast(U, self.discard(cast(type, t), cm))

    def discard_recover(self, t: type, cm: CodeMap) -> TreeRecovered:
        return TreeRecovered(self.discard(t, cm))

    def discard(self, t: type, cm: CodeMap) -> TreeError:
        self.error(cm)
        rvl = [] # type: List[TreeError]
        while True:
            elem = cast(Union[Token, TokenTree], self.pop())
            assert isinstance(elem, Union[Token, TokenTree]), type(elem).__name__
            rvl.append(make_error(elem, nested=True))
            if isinstance(elem, t):
                # could be the sentinel
                break
            if elem is self.sentinel:
                # avoid the "reuse sentinel" assert in peek()
                assert self.it is None and self.cache is None
                self.cache = self.sentinel
                rvl.pop()
                break
        return TreeError(*rvl)

    def peek_if(self, t: Callable[..., U]) -> Optional[U]:
        rv = self.peek().head()
        if isinstance(rv, cast(type, t)):
            return cast(U, rv)
        ts = getattr(t, '__union_params__', [t])
        for t in ts:
            assert t.__name__ != 'Union', repr(t)
            self.expected.add(t.__name__)
        return None

    def pop_if(self, t: Callable[..., U]) -> Optional[U]:
        rv = self.peek_if(t)
        if rv is not None:
            self.pop()
        return rv

    def cut(self, *t: type, killswitch: Tuple[type, ...] = ()) -> Tuple['InnerPeekable', HasToken]:
        # be sure to call uncut() if you don't exhaust the slave
        expected = self.expected.copy()
        contents = [] # type: List[HasToken]
        while True:
            sentinel = self.pop()
            if isinstance(sentinel, killswitch):
                t = ()
            if isinstance(sentinel, t):
                fin = sentinel
                break
            if sentinel is self.sentinel:
                fin = None
                break
            contents.append(sentinel)
        rv = InnerPeekable(contents, sentinel, self)
        rv.expected = expected
        return rv, fin

    def uncut(self, other: 'InnerPeekable') -> None:
        # only necessary if you don't exhaust the slave
        assert other.parent is self, '%r != %r' % (other.parent, self)
        assert self.it is None, repr(list(self.it))
        self.it = other.it
        other.it = None
        assert self.cache is None, repr(self.cache)
        self.cache = other.cache
        other.cache = None
        self.expected = other.expected

    def nuke(self) -> None:
        # eek!
        self.it = None

    def error(self, cm: CodeMap) -> None:
        actual = self.peek().head()
        actual_str = actual.__class__.__name__
        expected_str = ', '.join(sorted(self.expected))
        cm.error(actual._span, 'Unexpected %s, expected one of {%s}.' % (actual_str, expected_str))

    def exit(self) -> 'BasePeekable':
        assert self.parent is not None
        return self.parent

    @staticmethod
    def from_token_file(tf: TokenFile) -> 'OuterPeekable':
        return OuterPeekable(tf._contents, tf._terminator, parent=None)

class OuterPeekable(BasePeekable):
    # TODO not currently used, see do_parse_block
    def enter_token_block(self) -> Tuple[TokIndent, 'OuterPeekable']:
        tb = cast(TokenBlock, self.pop())
        assert isinstance(tb, TokenBlock), type(tb).__name__
        assert isinstance(tb.head(), TokIndent), type(tb.head()).__name__
        return cast(TokIndent, tb._open), OuterPeekable(tb._inner, tb._close, parent=self)

    def enter_token_line(self) -> 'InnerPeekable':
        expected = self.expected.copy()
        tl = cast(TokenLine, self.pop())
        self.expected.update(expected)
        assert isinstance(tl, TokenLine), type(tl).__name__
        return InnerPeekable(tl._contents, tl._terminator, parent=self)

class InnerPeekable(BasePeekable):
    def enter_token_tree(self, head_ty: Callable[..., U]) -> Tuple[U, 'InnerPeekable']:
        tt = cast(TokenTree, self.pop())
        assert isinstance(tt, TokenTree), type(tt).__name__
        assert isinstance(tt.head(), cast(type, head_ty)), type(tt.head()).__name__
        return cast(U, tt._open), InnerPeekable(tt._inner, tt._close, parent=self)

# Invariant:
#   Every call to TreeError() shall be preceded by a call to
#   self.cm.error(). Note that there has already been such a call if
#   it contains a TreeError or TokError.
#   The call to self.cm.error() shall occur before any recovery parsing.

def recover(*args: TokenOrTree) -> TreeRecovered:
    assert all(isinstance(a, TokenOrTree) for a in args), repr(args)
    return TreeRecovered(TreeError(*args))

def maybe_recover(a: T) -> T:
    if isinstance(a, TreeError):
        a = cast(T, TreeRecovered(a))
    return a

def make_error_list(args: List[Union[Token, TokenTree]]) -> TreeErrorList:
    return TreeErrorList([make_error(a, nested=True) for a in args])

def make_error(arg: Union[Token, TokenTree], *, nested: bool = False) -> TreeError:
    if isinstance(arg, TokenTree):
        return TreeError(arg._open, make_error_list(arg._inner), arg._close)
    elif not nested:
        return TreeError(arg)
    else:
        return cast(TreeError, arg)

class Parser:
    def __init__(self, cm: CodeMap, dialect: Dialect) -> None:
        self.cm = cm
        self.dialect = dialect

    def unexpected(self, t: HasToken) -> None:
        if not isinstance(t, TokError):
            self.cm.error(t._span, 'unexpected %s' % t.__class__.__name__)

    def skip(self, pkl: InnerPeekable, skipped: List[Token]) -> None:
        for expected in skipped:
            actual = pkl.expect(cast(Callable[..., Token], type(expected)))
            assert expected is actual, '%s != %s' % (expected, actual)

    def parse(self, name: str, txt: Union[str, bytes]) -> Tuple[FileInput, FileInfo, Optional[str], Dialect]:
        dialect = self.dialect
        if isinstance(txt, bytes):
            encoding = infer_encoding(txt, dialect.default_encoding)
            if encoding == 'mypy':
                dialect = dialect.add_future('mypy-codec')
                encoding = 'utf-8'
            txt = txt.decode(encoding)
        else:
            encoding = None
        file_info = self.cm.add(name, txt)
        lexer = Lexer(self.cm, file_info, dialect)
        dialect = lexer._dialect
        token_file = lexer.pull_file()
        rv = self.do_parse_file_input(token_file)
        return rv, file_info, encoding, dialect

    def do_parse_file_input(self, token_file: TokenFile) -> FileInput:
        _check_assert_messages()
        pkb = OuterPeekable.from_token_file(token_file)
        return self.parse_file_input(pkb)

    # Blocks

    def parse_file_input(self, pkb: OuterPeekable) -> FileInput:
        # This is very similar to parse_block but terminated by TokEof not TokDedent.
        stmts = [] # type: List[Stmt]
        while True:
            # make assertions happen
            gc.collect()

            eof = pkb.pop_if(TokEof)
            if eof is not None:
                return FileInput(stmts, eof)
            stmts.append(self.parse_stmt(pkb))

    def parse_stmt(self, pkb: OuterPeekable) -> Stmt:
        # not using peek_if because this should not be in 'expected'
        t = pkb.peek()
        # note, this is repeated after decorators have been parsed
        if isinstance(t, TokenBlock):
            pkb.pop()
            self.cm.error(t._span, 'unexpected indented block')
            stmts = self.do_parse_block(t)
            return recover(t._open, TreeList(stmts), t._close)
        if isinstance(t, Token):
            self.unexpected(t)
            return recover(t)

        if pkb.peek_if(KwIf):
            return self.parse_if_stmt(pkb)
        if pkb.peek_if(KwWhile):
            return self.parse_while_stmt(pkb)
        if pkb.peek_if(KwFor):
            return self.parse_for_stmt(pkb)
        if pkb.peek_if(KwTry):
            return self.parse_try_stmt(pkb)
        if pkb.peek_if(KwWith):
            return self.parse_with_stmt(pkb)
        decorators = [] # type: List[Decorator]
        while pkb.peek_if(OpAt):
            decorators.append(self.parse_decorator(pkb.enter_token_line()))

        if decorators:
            # repeated from before
            t = pkb.peek()
            if isinstance(t, TokenBlock):
                pkb.pop()
                self.cm.error(t._span, 'unexpected indented block')
                stmts = self.do_parse_block(t)
                rve = recover(t._open, TreeList(stmts), t._close)
                if decorators:
                    return recover(TreeList(decorators), rve)
                return rve
            if isinstance(t, Token):
                self.unexpected(t)
                rve = recover(t)
                if decorators:
                    return recover(TreeList(decorators), rve)
                return rve

        if pkb.peek_if(KwDef):
            rvf = self.parse_func_def(pkb)
            if decorators:
                return Decorated(decorators, rvf)
            return rvf
        if pkb.peek_if(KwClass):
            rvc = self.parse_class_def(pkb)
            if decorators:
                return Decorated(decorators, rvc)
            return rvc
        if pkb.peek_if(KwAsync):
            tl = cast(TokenLine, t)
            pktmp = BasePeekable(tl._contents, tl._terminator, parent=None)
            kw_async = pktmp.expect(KwAsync)
            if pktmp.peek_if(KwDef):
                pktmp.nuke()
                rva = AsyncFuncDef(kw_async, self.parse_func_def(pkb, skipped=[kw_async]))
                if decorators:
                    return Decorated(decorators, rva)
                return rva
            if not decorators:
                if pktmp.peek_if(KwFor):
                    pktmp.nuke()
                    return AsyncForStmt(kw_async, self.parse_for_stmt(pkb, skipped=[kw_async]))
                if pktmp.peek_if(KwWith):
                    pktmp.nuke()
                    return AsyncWithStmt(kw_async, self.parse_with_stmt(pkb, skipped=[kw_async]))
            pktmp.error(self.cm)
            pktmp.nuke()
            rve = pkb.enter_token_line().discard_recover(TokNewline, self.cm)
            if decorators:
                return recover(TreeList(decorators), rve)
            return rve
        if decorators:
            pkb.error(self.cm)
            # Does *not* consume the following (presumed) SimpleStmt.
            return recover(*decorators)
        # Bare suite errors.
        # Note that some of these won't happen. In this case,
        # the "expected" set will contain them for the error message.
        any_simple_stmt_message = '<any simple statement>'
        pkb.expected.add(any_simple_stmt_message)
        if isinstance(t.head(), KwElif):
            pkb.error(self.cm)
            return recover(self.parse_elif_suite(pkb))
        if isinstance(t.head(), KwElse):
            pkb.error(self.cm)
            return recover(self.parse_else_suite(pkb))
        if isinstance(t.head(), KwExcept):
            pkb.error(self.cm)
            return recover(self.parse_except_suite(pkb))
        if isinstance(t.head(), KwFinally):
            pkb.error(self.cm)
            return recover(self.parse_finally_suite(pkb))
        pkb.expected.remove(any_simple_stmt_message)
        return self.parse_simple_stmt(pkb.enter_token_line())

    def do_parse_block(self, tb: TokenBlock) -> List[Stmt]:
        pkb = OuterPeekable(tb._inner, tb._close, parent=None)
        return self.parse_block(pkb)

    def parse_block(self, pkb: OuterPeekable) -> List[Stmt]:
        # This is very similar to parse_file_input but terminated by TokDedent not TokEof.
        stmts = [] # type: List[Stmt]
        while True:
            eob = pkb.pop_if(TokDedent)
            if eob is not None:
                return stmts
            stmts.append(self.parse_stmt(pkb))

    def parse_if_stmt(self, pkb: OuterPeekable) -> IfStmt:
        if_suite = self.parse_if_suite(pkb)
        elif_suites = [] # type: List[ElifSuite]
        while pkb.peek_if(KwElif):
            elif_suites.append(self.parse_elif_suite(pkb))
        if pkb.peek_if(KwElse):
            else_suite = self.parse_else_suite(pkb)
        else:
            else_suite = None
        return IfStmt(if_suite, elif_suites, else_suite)

    def parse_while_stmt(self, pkb: OuterPeekable) -> WhileStmt:
        while_suite = self.parse_while_suite(pkb)
        if pkb.peek_if(KwElse):
            else_suite = self.parse_else_suite(pkb)
        else:
            else_suite = None
        return WhileStmt(while_suite, else_suite)

    def parse_for_stmt(self, pkb: OuterPeekable, skipped: List[Token] = []) -> ForStmt:
        pkl = pkb.enter_token_line()
        self.skip(pkl, skipped)

        for_suite = self.parse_for_suite(pkb)
        if pkb.peek_if(KwElse):
            else_suite = self.parse_else_suite(pkb)
        else:
            else_suite = None
        return ForStmt(for_suite, else_suite)

    def parse_try_stmt(self, pkb: OuterPeekable) -> TryStmt:
        try_suite = self.parse_try_suite(pkb)
        except_suites = [] # type: List[ExceptSuite]
        while pkb.peek_if(KwExcept):
            except_suites.append(self.parse_except_suite(pkb))
        if pkb.peek_if(KwElse):
            else_suite = self.parse_else_suite(pkb)
        else:
            else_suite = None
        if pkb.peek_if(KwFinally):
            finally_suite = self.parse_finally_suite(pkb)
        else:
            finally_suite = None
        return TryStmt(try_suite, except_suites, else_suite, finally_suite)

    def parse_with_stmt(self, pkb: OuterPeekable, skipped: List[Token] = []) -> WithStmt:
        pkl = pkb.enter_token_line()
        self.skip(pkl, skipped)

        with_kw = pkl.expect(KwWith)
        with_items = self.parse_with_items(pkl)
        colon, suite = self.parse_colon_suite(pkl)
        return WithStmt(with_kw, with_items, colon, suite)

    def parse_decorator(self, pkl: InnerPeekable) -> Decorator:
        at = pkl.expect(OpAt)
        name = self.parse_dotted_name(pkl)
        if pkl.peek_if(OpLPar):
            lpar, pkt = pkl.enter_token_tree(OpLPar)
            args = self.parse_arg_list(pkt)
            rpar = pkt.ensure(OpRPar, self.cm)
        else:
            lpar = None
            args = None
            rpar = None
        nl = pkl.ensure(TokNewline, self.cm)
        return Decorator(at, name, lpar, args, rpar, nl)

    def parse_func_def(self, pkb: OuterPeekable, skipped: List[Token] = []) -> FuncDef:
        pkl = pkb.enter_token_line()
        self.skip(pkl, skipped)

        kw_def = pkl.expect(KwDef)
        name = pkl.ensure(TokName, self.cm)
        parameters = self.parse_parameters(pkl)
        if pkl.peek_if(OpRArrow):
            r_arrow = pkl.ensure(OpRArrow, self.cm)
            return_annotation = self.parse_test(pkl, sloppy=True)
        else:
            r_arrow = None
            return_annotation = None
        colon, suite = self.parse_colon_suite(pkl)
        return FuncDef(kw_def, name, parameters, r_arrow, return_annotation, colon, suite)

    def parse_class_def(self, pkb: OuterPeekable) -> ClassDef:
        pkl = pkb.enter_token_line()

        kw_class = pkl.expect(KwClass)
        name = pkl.ensure(TokName, self.cm)
        if pkl.peek_if(OpLPar):
            open, pkt = pkl.enter_token_tree(OpLPar)
            bases = self.parse_arg_list(pkt)
            close = pkt.ensure(OpRPar, self.cm)
        else:
            open = None
            bases = None
            close = None
        colon, suite = self.parse_colon_suite(pkl)
        return ClassDef(kw_class, name, open, bases, close, colon, suite)

    def parse_if_suite(self, pkb: OuterPeekable) -> IfSuite:
        pkl = pkb.enter_token_line()

        if_kw = pkl.expect(KwIf)
        condition = self.parse_test(pkl, sloppy=True)
        colon, suite = self.parse_colon_suite(pkl)
        return IfSuite(if_kw, condition, colon, suite)

    def parse_elif_suite(self, pkb: OuterPeekable) -> ElifSuite:
        pkl = pkb.enter_token_line()

        elif_kw = pkl.expect(KwElif)
        condition = self.parse_test(pkl, sloppy=True)
        colon, suite = self.parse_colon_suite(pkl)
        return ElifSuite(elif_kw, condition, colon, suite)

    def parse_else_suite(self, pkb: OuterPeekable) -> ElseSuite:
        pkl = pkb.enter_token_line()

        else_kw = pkl.expect(KwElse)
        colon, suite = self.parse_colon_suite(pkl)
        return ElseSuite(else_kw, colon, suite)

    def parse_while_suite(self, pkb: OuterPeekable) -> WhileSuite:
        pkl = pkb.enter_token_line()

        while_kw = pkl.expect(KwWhile)
        condition = self.parse_test(pkl, sloppy=True)
        colon, suite = self.parse_colon_suite(pkl)
        return WhileSuite(while_kw, condition, colon, suite)

    def parse_for_suite(self, pkb: OuterPeekable) -> ForSuite:
        pkl = pkb.enter_token_line()

        for_kw = pkl.expect(KwFor)
        targets = self.parse_target_list(pkl)
        in_kw = pkl.ensure(KwIn, self.cm)
        tests = self.parse_test_list(pkl)
        colon, suite = self.parse_colon_suite(pkl)
        return ForSuite(for_kw, targets, in_kw, tests, colon, suite)

    def parse_try_suite(self, pkb: OuterPeekable) -> TrySuite:
        pkl = pkb.enter_token_line()

        try_kw = pkl.expect(KwTry)
        colon, suite = self.parse_colon_suite(pkl)
        return TrySuite(try_kw, colon, suite)

    def parse_except_suite(self, pkb: OuterPeekable) -> ExceptSuite:
        pkl = pkb.enter_token_line()

        except_kw = pkl.expect(KwExcept)
        if not pkl.peek_if(OpColon):
            test = self.parse_test(pkl)
            as_kw = cast(Union[KwAs, OpComma, None], pkl.pop_if(KwAs) or pkl.pop_if(OpComma))
            if as_kw is not None:
                name = pkl.ensure(TokName, self.cm)
        else:
            test = None
            as_kw = None
            name = None
        colon, suite = self.parse_colon_suite(pkl)

        except_clause = ExceptClause(except_kw, test, as_kw, name)
        return ExceptSuite(except_clause, colon, suite)

    def parse_finally_suite(self, pkb: OuterPeekable) -> FinallySuite:
        pkl = pkb.enter_token_line()

        finally_kw = pkl.expect(KwFinally)
        colon, suite = self.parse_colon_suite(pkl)
        return FinallySuite(finally_kw, colon, suite)

    # Statements

    def parse_simple_stmt(self, pkl: InnerPeekable) -> SimpleStmt:
        statements = [] # type: List[SmallStmtAndOpSemicolon]
        while True:
            pks, _ = pkl.cut(OpSemicolon)
            ss = self.parse_small_stmt(pks)
            if not pks.peek_if(TokNewline):
                sc = pks.ensure(OpSemicolon, self.cm)
            else:
                sc = None
            statements.append(SmallStmtAndOpSemicolon(ss, sc))
            if pks.peek_if(TokNewline):
                break
        nl = pks.expect(TokNewline)
        return maybe_recover(SimpleStmt(statements, nl))

    def parse_small_stmt(self, pkl: InnerPeekable) -> SmallStmt:
        if pkl.peek_if(KwPrint):
            return self.parse_print_stmt(pkl)
        if pkl.peek_if(KwDel):
            return self.parse_del_stmt(pkl)
        if pkl.peek_if(KwPass):
            return self.parse_pass_stmt(pkl)
        if pkl.peek_if(KwBreak):
            return self.parse_break_stmt(pkl)
        if pkl.peek_if(KwContinue):
            return self.parse_continue_stmt(pkl)
        if pkl.peek_if(KwReturn):
            return self.parse_return_stmt(pkl)
        if pkl.peek_if(KwRaise):
            return self.parse_raise_stmt(pkl)
        if pkl.peek_if(KwYield):
            return self.parse_yield_stmt(pkl)
        if pkl.peek_if(KwImport):
            return self.parse_import_stmt(pkl)
        if pkl.peek_if(KwFrom):
            return self.parse_from_stmt(pkl)
        if pkl.peek_if(KwGlobal):
            return self.parse_global_stmt(pkl)
        if pkl.peek_if(KwNonlocal):
            return self.parse_nonlocal_stmt(pkl)
        if pkl.peek_if(KwExec):
            return self.parse_exec_stmt(pkl)
        if pkl.peek_if(KwAssert):
            return self.parse_assert_stmt(pkl)

        # AnyExprStmt
        lhses = [] # type: List[Tuple[Union[AugTarget, TargetList], Token]]
        while True:
            # This was the motivating factor for the entire design.
            # Note that it *only* works because of token trees.
            pklhs, peek_fin = pkl.cut(OpEqual, Augassign, killswitch=(KwLambda,))
            if isinstance(peek_fin, OpEqual):
                lhses.append((self.parse_target_list(pklhs), pklhs.ensure(cast(Callable[..., Token], type(peek_fin)), self.cm)))
                continue
            if peek_fin is not None:
                lhses.append((self.parse_aug_target(pklhs), pklhs.ensure(cast(Callable[..., Token], type(peek_fin)), self.cm)))
                continue
            # cut reached the sentinel - rhs
            break
        if not lhses:
            rhs = self.parse_test_list(pklhs)
            # Ensure that the TokNewline is available to the right caller
            pkl.uncut(pklhs)
            return ExprStmt(rhs)
        rhs = self.parse_yield_expr_or_test_list(pklhs)
        # Ensure that the TokNewline is available to the right caller
        pkl.uncut(pklhs)
        if len(lhses) == 1 and not isinstance(lhses[0][1], OpEqual):
            return AugmentedAssignStmt(lhses[0][0], cast(Augassign, lhses[0][1]), rhs)
        # Chained assignment. If any aug assign, error.
        target_lists = [] # type: List[TargetListAndOpEqual]
        for target, op in lhses:
            if isinstance(op, OpEqual):
                target_lists.append(TargetListAndOpEqual(target, op))
            else:
                target_lists.append(cast(TargetListAndOpEqual, TreeError(target, op)))
        return AssignStmt(target_lists, rhs)

    def parse_with_items(self, pkl: InnerPeekable) -> List[WithItemAndOpComma]:
        rv = [] # type: List[WithItemAndOpComma]
        tmp = self.parse_with_item(pkl)
        while pkl.peek_if(OpComma):
            rv.append(WithItemAndOpComma(tmp, pkl.expect(OpComma)))
            tmp = self.parse_with_item(pkl)
        rv.append(WithItemAndOpComma(tmp, None))
        return rv

    def parse_colon_suite(self, pkl: InnerPeekable) -> Tuple[OpColon, Suite]:
        colon = pkl.ensure(OpColon, self.cm)
        if pkl.peek_if(TokNewline):
            return (colon, self.parse_complex_suite(pkl))
        else:
            return (colon, self.parse_simple_stmt(pkl))

    def parse_dotted_name(self, pkl: InnerPeekable) -> DottedName:
        name = pkl.ensure(TokName, self.cm)
        dot_names = [] # type: List[OpDotAndTokName]
        while pkl.peek_if(OpDot):
            dot_names.append(OpDotAndTokName(pkl.expect(OpDot), pkl.ensure(TokName, self.cm)))
        return DottedName(name, dot_names)

    def parse_arg_list(self, pkl: InnerPeekable) -> ArgList:
        arguments = [] # type: List[ArgumentAndOpComma]
        while not pkl.peek_if(OpRPar):
            argument = self.parse_argument(pkl)
            comma = pkl.pop_if(OpComma)
            arguments.append(ArgumentAndOpComma(argument, comma))
            if not comma:
                break
        return ArgList(arguments)

    def parse_parameters(self, pkl_: InnerPeekable) -> Parameters:
        if not pkl_.peek_if(OpLPar):
            pkl_.error(self.cm)
            return cast(Parameters, make_error(cast(Union[Token, TokenTree], pkl_.pop())))
        open, pkt = pkl_.enter_token_tree(OpLPar)
        if pkt.peek_if(OpRPar):
            close = pkt.expect(OpRPar)
            return Parameters(open, None, close)
        typed_args = self.parse_typed_args_list(pkt)
        close = pkt.ensure(OpRPar, self.cm)
        return Parameters(open, typed_args, close)

    def parse_target_list(self, pkl: InnerPeekable) -> TargetList:
        # point of Target vs CommaTarget:
        #   a = b
        #   a, = b
        target = self.parse_target(pkl)
        if not pkl.peek_if(OpComma):
            return target
        targets = [] # type: List[TargetAndOpComma]
        while True:
            comma = pkl.pop_if(OpComma)
            targets.append(TargetAndOpComma(target, comma))
            if not comma:
                break
            if isinstance(pkl.peek(), (KwIn, OpRPar, OpRSqb, OpEqual, OpSemicolon, TokNewline)):
                break
            target = self.parse_target(pkl)
        return CommaTarget(targets)

    def parse_print_stmt(self, pkl: InnerPeekable) -> PrintStmt:
        kw_print = pkl.expect(KwPrint)
        r_shift = pkl.pop_if(OpRShift)
        if r_shift is not None:
            stream = self.parse_test(pkl)
            comma = pkl.pop_if(OpComma)
            if comma is None:
                return PrintStmt(kw_print, r_shift, stream, comma, [])
        else:
            stream = None
            comma = None
        exprs = [] # type: List[TestAndOpComma]
        while not isinstance(pkl.peek(), (OpSemicolon, TokNewline)):
            test = self.parse_test(pkl)
            tcomma = pkl.pop_if(OpComma)
            exprs.append(TestAndOpComma(test, tcomma))
            if tcomma is None:
                break
        return PrintStmt(kw_print, r_shift, stream, comma, exprs)

    def parse_del_stmt(self, pkl: InnerPeekable) -> DelStmt:
        kw_del = pkl.expect(KwDel)
        targets = self.parse_target_list(pkl)
        return DelStmt(kw_del, targets)

    def parse_pass_stmt(self, pkl: InnerPeekable) -> PassStmt:
        return PassStmt(pkl.expect(KwPass))

    def parse_break_stmt(self, pkl: InnerPeekable) -> BreakStmt:
        return BreakStmt(pkl.expect(KwBreak))

    def parse_continue_stmt(self, pkl: InnerPeekable) -> ContinueStmt:
        return ContinueStmt(pkl.expect(KwContinue))

    def parse_return_stmt(self, pkl: InnerPeekable) -> ReturnStmt:
        kw_return = pkl.expect(KwReturn)
        if not isinstance(pkl.peek(), (OpSemicolon, TokNewline)):
            tests = self.parse_test_list(pkl)
        else:
            tests = None
        return ReturnStmt(kw_return, tests)

    def parse_raise_stmt(self, pkl: InnerPeekable) -> RaiseStmt:
        kw_raise = pkl.expect(KwRaise)
        if not isinstance(pkl.peek(), (OpSemicolon, TokNewline)):
            test = self.parse_test(pkl)
        else:
            return SimpleRaiseStmt(kw_raise, None)
        kw_from = pkl.pop_if(KwFrom)
        if kw_from is not None:
            context = self.parse_test(pkl)
            return RaiseFromStmt(kw_raise, test, kw_from, context)
        comma1 = pkl.pop_if(OpComma)
        if comma1 is None:
            return SimpleRaiseStmt(kw_raise, test)
        value = self.parse_test(pkl)
        comma2 = pkl.pop_if(OpComma)
        if comma2 is not None:
            traceback = self.parse_test(pkl)
        else:
            traceback = None
        return LegacyRaiseStmt(kw_raise, test, comma1, value, comma2, traceback)

    def parse_yield_stmt(self, pkl: InnerPeekable) -> YieldStmt:
        return YieldStmt(self.parse_yield_expr(pkl))

    def parse_import_stmt(self, pkl: InnerPeekable) -> ImportName:
        kw_import = pkl.expect(KwImport)
        dotted_as_names = self.parse_dotted_as_names(pkl)
        return ImportName(kw_import, dotted_as_names)

    def parse_from_stmt(self, pkl: InnerPeekable) -> Union[ImportFrom, ImportAll]:
        kw_from = pkl.expect(KwFrom)
        dots = self.parse_any_dots_list(pkl)
        if not pkl.peek_if(KwImport):
            dotted_name = self.parse_dotted_name(pkl)
        else:
            dotted_name = None
        kw_import = pkl.ensure(KwImport, self.cm)
        if not dots and not dotted_name:
            kw_import = cast(KwImport, TreeError(kw_import))
        star = pkl.pop_if(OpStar)
        if star is not None:
            return ImportAll(kw_from, dots, dotted_name, kw_import, star)
        if pkl.peek_if(OpLPar):
            open, pkl = pkl.enter_token_tree(OpLPar)
        else:
            open = None
        names = self.parse_import_as_names(pkl)
        if open:
            close = pkl.ensure(OpRPar, self.cm)
        else:
            close = None
        return ImportFrom(kw_from, dots, dotted_name, kw_import, open, names, close)

    def parse_global_stmt(self, pkl: InnerPeekable) -> GlobalStmt:
        kw_global = pkl.expect(KwGlobal)
        names = [] # type: List[TokNameAndOpComma]
        name = pkl.ensure(TokName, self.cm)
        while pkl.peek_if(OpComma):
            names.append(TokNameAndOpComma(name, pkl.expect(OpComma)))
            name = pkl.ensure(TokName, self.cm)
        names.append(TokNameAndOpComma(name, None))
        return GlobalStmt(kw_global, names)

    def parse_nonlocal_stmt(self, pkl: InnerPeekable) -> NonlocalStmt:
        kw_nonlocal = pkl.expect(KwNonlocal)
        names = [] # type: List[TokNameAndOpComma]
        name = pkl.ensure(TokName, self.cm)
        while pkl.peek_if(OpComma):
            names.append(TokNameAndOpComma(name, pkl.expect(OpComma)))
            name = pkl.ensure(TokName, self.cm)
        names.append(TokNameAndOpComma(name, None))
        return NonlocalStmt(kw_nonlocal, names)

    def parse_exec_stmt(self, pkl: InnerPeekable) -> ExecStmt:
        kw_exec = pkl.expect(KwExec)
        or_expr = self.parse_or_expr(pkl)
        if pkl.peek_if(KwIn):
            kw_in = pkl.expect(KwIn)
            globals = self.parse_test(pkl)
        else:
            kw_in = None
            globals = None
        if kw_in is not None and pkl.peek_if(OpComma):
            comma = pkl.expect(OpComma)
            locals = self.parse_test(pkl)
        else:
            comma = None
            locals = None
        return ExecStmt(kw_exec, or_expr, kw_in, globals, comma, locals)

    def parse_assert_stmt(self, pkl: InnerPeekable) -> AssertStmt:
        kw_assert = pkl.expect(KwAssert)
        test = self.parse_test(pkl)
        if pkl.peek_if(OpComma):
            comma = pkl.expect(OpComma)
            message = self.parse_test(pkl)
        else:
            comma = None
            message = None
        return AssertStmt(kw_assert, test, comma, message)

    def parse_aug_target(self, pkl: InnerPeekable) -> AugTarget:
        # Note, this is called as a fallback from parse_target.
        # Parse any OrExpr, but discard if not name/subscript/attr
        # Special-case parenthesized target.
        if pkl.peek_if(OpLPar) and not pkl.it.__length_hint__(): # type: ignore
            open, pkl = pkl.enter_token_tree(OpLPar)
            target = self.parse_aug_target(pkl)
            close = pkl.ensure(OpRPar, self.cm)
            return ParenAugTarget(open, target, close)
        test = self.parse_or_expr(pkl)
        if isinstance(test, AugTarget):
            return test
        self.cm.error(test._span, 'expression not allowed as lvalue')
        return cast(AugTarget, TreeError(test))

    def parse_with_item(self, pkl: InnerPeekable) -> WithItem:
        test = self.parse_test(pkl)
        if pkl.peek_if(KwAs):
            kw_as = pkl.expect(KwAs)
            target = self.parse_target(pkl)
        else:
            kw_as = None
            target = None
        return WithItem(test, kw_as, target)

    def parse_complex_suite(self, pkl: InnerPeekable) -> ComplexSuite:
        nl = pkl.expect(TokNewline)
        pkb = pkl.exit()
        assert isinstance(pkb, OuterPeekable), type(pkb).__name__
        if not pkb.peek_if(TokIndent):
            self.cm.error(pkb.peek()._span, 'expected indented block')
            return cast(ComplexSuite, TreeError(nl))
        block = cast(TokenBlock, pkb.pop())
        stmts = self.do_parse_block(block)
        return ComplexSuite(nl, cast(TokIndent, block._open), stmts, cast(TokDedent, block._close))

    def parse_argument(self, pkl: InnerPeekable) -> Argument:
        star = pkl.pop_if(OpStar)
        if star:
            test = self.parse_test(pkl)
            return StarTest(star, test)
        star_star = pkl.pop_if(OpStarStar)
        if star_star:
            test = self.parse_test(pkl)
            return StarStarTest(star_star, test)
        test = self.parse_test(pkl)
        equal = pkl.pop_if(OpEqual)
        if equal:
            if not isinstance(test, TokName):
                self.cm.error(test._span, 'keyword must be just name')
                test = TreeError(test)
            name = cast(TokName, test)
            test = self.parse_test(pkl)
            return KeywordArgument(name, equal, test)
        if pkl.peek_if(KwFor):
            comp_for = self.parse_comp_for(pkl)
        else:
            comp_for = None
        return PositionalArgument(test, comp_for)

    def parse_target(self, pkl: InnerPeekable) -> Target:
        if pkl.peek_if(OpLPar) and not pkl.it.__length_hint__(): # type: ignore
            open1, pkl = pkl.enter_token_tree(OpLPar)
            targets = self.parse_target_list(pkl)
            close1 = pkl.ensure(OpRPar, self.cm)
            return ParenTargetList(open1, targets, close1)
        if pkl.peek_if(OpLSqb) and not pkl.it.__length_hint__(): # type: ignore
            open2, pkl = pkl.enter_token_tree(OpLSqb)
            targets = self.parse_target_list(pkl)
            close2 = pkl.ensure(OpRSqb, self.cm)
            return SqbTargetList(open2, targets, close2)
        star = pkl.pop_if(OpStar)
        if star:
            return StarTarget(star, self.parse_target(pkl))
        # This union is a subset, except for ParenAugTarget.
        target = self.parse_aug_target(pkl)
        assert not isinstance(target, ParenAugTarget)
        return target

    def parse_dotted_as_names(self, pkl: InnerPeekable) -> List[DottedAsName]:
        rv = [] # type: List[DottedAsName]
        while not isinstance(pkl.peek(), (OpSemicolon, TokNewline)):
            dotted_name = self.parse_dotted_name(pkl)
            kw_as = pkl.pop_if(KwAs)
            if kw_as:
                name = pkl.ensure(TokName, self.cm)
            else:
                name = None
            comma = pkl.pop_if(OpComma)
            rv.append(DottedAsName(dotted_name, kw_as, name, comma))
            if not comma:
                break
        return rv

    def parse_any_dots_list(self, pkl: InnerPeekable) -> List[AnyDots]:
        rv = [] # type: List[AnyDots]
        while True:
            dot3 = pkl.pop_if(OpDotDotDot)
            if dot3:
                rv.extend([dot3, None, None])
            dot1 = pkl.pop_if(OpDot)
            if dot1:
                rv.append(dot1)
            if not dot3 and not dot1:
                break
        return rv

    def parse_import_as_names(self, pkl: InnerPeekable) -> List[ImportAsName]:
        rv = [] # type: List[ImportAsName]
        while not isinstance(pkl.peek(), (OpRPar, OpSemicolon, TokNewline)):
            orig_name = pkl.ensure(TokName, self.cm)
            kw_as = pkl.pop_if(KwAs)
            if kw_as:
                local_name = pkl.ensure(TokName, self.cm)
            else:
                local_name = None
            comma = pkl.pop_if(OpComma)
            rv.append(ImportAsName(orig_name, kw_as, local_name, comma))
            if not comma:
                break
        return rv

    def parse_typed_args_list(self, pkl: InnerPeekable) -> TypedArgsList:
        positional_args = [] # type: List[TypedArg]
        keyword_args = [] # type: List[TypedArg]

        while not isinstance(pkl.peek(), (OpRPar, OpStar, OpStarStar)):
            typed_arg = self.parse_typed_arg(pkl)
            positional_args.append(typed_arg)
            if not typed_arg.comma:
                break
        star = pkl.pop_if(OpStar)
        if star:
            if pkl.peek_if(OpComma):
                positional_arg_pack = self.parse_tfp_def(pkl)
            else:
                positional_arg_pack = None
            comma = pkl.pop_if(OpComma)
        else:
            positional_arg_pack = None
            comma = None
        if comma or not star:
            while not isinstance(pkl.peek(), (OpRPar, OpStarStar)):
                typed_arg = self.parse_typed_arg(pkl)
                keyword_args.append(typed_arg)
                if not typed_arg.comma:
                    break
            star_star = pkl.pop_if(OpStarStar)
            if star_star:
                keyword_arg_pack = self.parse_tfp_def(pkl)
            else:
                keyword_arg_pack = None
        else:
            star_star = None
            keyword_arg_pack = None
        return TypedArgsList(positional_args, star, positional_arg_pack, comma, keyword_args, star_star, keyword_arg_pack)

    def parse_typed_arg(self, pkl: InnerPeekable) -> TypedArg:
        target = self.parse_tfp_def(pkl)
        equal = pkl.pop_if(OpEqual)
        if equal:
            default = self.parse_test(pkl)
        else:
            default = None
        comma = pkl.pop_if(OpComma)
        return TypedArg(target, equal, default, comma)

    def parse_tfp_def(self, pkl: InnerPeekable) -> TfpDef:
        if pkl.peek_if(OpLPar):
            # This is only allowed with Dialect(2.7, mypy-codec).
            open, pkl = pkl.enter_token_tree(OpLPar)
            targets = [] # type: List[TfpDefAndOpComma]
            while not pkl.peek_if(OpRPar):
                tfp_def = self.parse_tfp_def(pkl)
                comma = pkl.pop_if(OpComma)
                targets.append(TfpDefAndOpComma(tfp_def, comma))
                if not comma:
                    break
            close = pkl.ensure(OpRPar, self.cm)
            return TfpList(open, targets, close)
        tname = self.parse_tname(pkl)
        return tname

    def parse_tname(self, pkl: InnerPeekable) -> Tname:
        name = pkl.ensure(TokName, self.cm)
        colon = pkl.pop_if(OpColon)
        if colon:
            annotation = self.parse_test(pkl)
        else:
            annotation = None
        return Tname(name, colon, annotation)

    def parse_var_args_list(self, pkl: InnerPeekable) -> VarArgsList:
        positional_args = [] # type: List[VarArg]
        keyword_args = [] # type: List[VarArg]

        while not isinstance(pkl.peek(), (OpColon, OpStar, OpStarStar)):
            var_arg = self.parse_var_arg(pkl)
            positional_args.append(var_arg)
            if not var_arg.comma:
                break
        star = pkl.pop_if(OpStar)
        if star:
            if not pkl.peek_if(OpComma):
                positional_arg_pack = self.parse_vfp_def(pkl)
            else:
                positional_arg_pack = None
            comma = pkl.pop_if(OpComma)
        else:
            positional_arg_pack = None
            comma = None
        if comma or not star:
            while not isinstance(pkl.peek(), (OpColon, OpStarStar)):
                var_arg = self.parse_var_arg(pkl)
                keyword_args.append(var_arg)
                if not var_arg.comma:
                    break
            star_star = pkl.pop_if(OpStarStar)
            if star_star:
                keyword_arg_pack = self.parse_vfp_def(pkl)
            else:
                keyword_arg_pack = None
        else:
            star_star = None
            keyword_arg_pack = None
        return VarArgsList(positional_args, star, positional_arg_pack, comma, keyword_args, star_star, keyword_arg_pack)

    def parse_var_arg(self, pkl: InnerPeekable) -> VarArg:
        target = self.parse_vfp_def(pkl)
        equal = pkl.pop_if(OpEqual)
        if equal:
            default = self.parse_test(pkl)
        else:
            default = None
        comma = pkl.pop_if(OpComma)
        return VarArg(target, equal, default, comma)

    def parse_vfp_def(self, pkl: InnerPeekable) -> VfpDef:
        if pkl.peek_if(OpLPar):
            # This is only allowed with Dialect(2.7, mypy-codec).
            open, pkl = pkl.enter_token_tree(OpLPar)
            targets = [] # type: List[VfpDefAndOpComma]
            while not pkl.peek_if(OpRPar):
                vfp_def = self.parse_vfp_def(pkl)
                comma = pkl.pop_if(OpComma)
                targets.append(VfpDefAndOpComma(vfp_def, comma))
                if not comma:
                    break
            close = pkl.ensure(OpRPar, self.cm)
            return VfpList(open, targets, close)
        vname = self.parse_vname(pkl)
        return vname

    def parse_vname(self, pkl: InnerPeekable) -> Vname:
        name = pkl.ensure(TokName, self.cm)
        return Vname(name)

    # Expressions

    def parse_yield_expr_or_test_list(self, pkl: InnerPeekable) -> YieldExprOrTestList:
        if pkl.peek_if(KwYield):
            return self.parse_yield_expr(pkl)
        else:
            return self.parse_test_list(pkl)

    def parse_yield_expr(self, pkl: InnerPeekable) -> YieldExpr:
        kw_yield = pkl.expect(KwYield)
        if pkl.peek_if(KwFrom):
            kw_from = pkl.expect(KwFrom)
            test = self.parse_test(pkl, sloppy=True)
            return YieldFromExpr(kw_yield, kw_from, test)
        if not pkl.peek_if(Union[OpRPar, OpSemicolon, TokNewline]):
            tests = self.parse_test_list(pkl)
        else:
            tests = None
        return SimpleYieldExpr(kw_yield, tests)

    def parse_test_list(self, pkl: InnerPeekable) -> TestList:
        tests = [] # type: List[TestAndOpComma]
        while not pkl.peek_if(Union[OpSemicolon, TokNewline, OpBacktick, OpColon]):
            test = self.parse_test(pkl)
            comma = pkl.pop_if(OpComma)
            tests.append(TestAndOpComma(test, comma))
            if not comma:
                break
        if len(tests) == 1 and isinstance(tests[0], TestAndOpComma) and not tests[0].comma:
            return tests[0].test
        return CommaExpr(tests)

    def parse_test_list_nocond(self, pkl: InnerPeekable) -> TestListNocond:
        tests = [] # type: List[TestNocondAndOpComma]
        while not pkl.peek_if(Union[OpSemicolon, TokNewline, OpBacktick, OpColon]):
            test = self.parse_test_nocond(pkl)
            comma = pkl.pop_if(OpComma)
            tests.append(TestNocondAndOpComma(test, comma))
            if not comma:
                break
        if len(tests) == 1 and not tests[0].comma:
            return tests[0].test
        return CommaExprNocond(tests)

    def parse_test(self, pkl: InnerPeekable, *, sloppy=False) -> Test:
        if sloppy:
            rvl = self.parse_test_list(pkl)
            if isinstance(rvl, CommaExpr):
                self.cm.error(rvl._span, 'comma expression not allowed here')
                rvl = recover(rvl)
            return rvl
        kw_lambda = pkl.pop_if(KwLambda)
        if kw_lambda:
            if not pkl.peek_if(OpColon):
                args = self.parse_var_args_list(pkl)
            else:
                args = None
            colon = pkl.ensure(OpColon, self.cm)
            body = self.parse_test(pkl)
            return LambdaExpr(kw_lambda, args, colon, body)
        value_if_true = self.parse_or_test(pkl)
        kw_if = pkl.pop_if(KwIf)
        if not kw_if:
            return value_if_true
        condition = self.parse_or_test(pkl)
        kw_else = pkl.ensure(KwElse, self.cm)
        value_if_false = self.parse_test(pkl)
        return TernaryExpr(value_if_true, kw_if, condition, kw_else, value_if_false)

    def parse_test_nocond(self, pkl: InnerPeekable, *, sloppy=False) -> TestNocond:
        if sloppy:
            rvl = self.parse_test_list_nocond(pkl)
            if isinstance(rvl, CommaExprNocond):
                self.cm.error(rvl._span, 'comma expression not allowed here')
                rvl = recover(rvl)
            return rvl
        kw_lambda = pkl.pop_if(KwLambda)
        if kw_lambda:
            if not pkl.peek_if(OpColon):
                args = self.parse_var_args_list(pkl)
            else:
                args = None
            colon = pkl.ensure(OpColon, self.cm)
            body = self.parse_test_nocond(pkl)
            return LambdaExprNocond(kw_lambda, args, colon, body)
        value_if_true = self.parse_or_test(pkl)
        # no cond!
        return value_if_true

    def parse_or_test(self, pkl: InnerPeekable) -> AnyOrTest:
        lhs = self.parse_and_test(pkl) # type: AnyOrTest
        while pkl.peek_if(KwOr):
            kw_or = pkl.expect(KwOr)
            rhs = self.parse_and_test(pkl)
            lhs = OrTest(lhs, kw_or, rhs)
        return lhs

    def parse_and_test(self, pkl: InnerPeekable) -> AnyAndTest:
        lhs = self.parse_not_test(pkl) # type: AnyAndTest
        while pkl.peek_if(KwAnd):
            kw_and = pkl.expect(KwAnd)
            rhs = self.parse_not_test(pkl)
            lhs = AndTest(lhs, kw_and, rhs)
        return lhs

    def parse_not_test(self, pkl: InnerPeekable) -> AnyNotTest:
        kw_not = pkl.pop_if(KwNot)
        if kw_not:
            rhs = self.parse_not_test(pkl)
            return NotTest(kw_not, rhs)
        lhs = self.parse_comparison(pkl)
        return lhs

    def maybe_parse_comp_op(self, pkl: InnerPeekable) -> Optional[CompOp]:
        simple_op = pkl.pop_if(Union[OpLess, OpGreater, OpEqEqual, OpGreaterEqual, OpLessEqual, OpNotEqual])
        if simple_op:
            return simple_op
        in_op = pkl.pop_if(KwIn)
        if in_op:
            return in_op
        not_op = pkl.pop_if(KwNot)
        if not_op:
            in_op = pkl.ensure(KwIn, self.cm)
            return KwsNotIn(not_op, in_op)
        is_op = pkl.pop_if(KwIs)
        if is_op:
            not_op = pkl.pop_if(KwNot)
            if not_op:
                return KwsIsNot(is_op, not_op)
            return is_op
        return None

    def parse_comparison(self, pkl: InnerPeekable) -> AnyComparison:
        lhs = self.parse_or_expr(pkl)
        rhses = [] # type: List[CompOpAndOrExpr]
        while True:
            comp_op = self.maybe_parse_comp_op(pkl)
            if not comp_op:
                break
            rhs = self.parse_or_expr(pkl)
            rhses.append(CompOpAndOrExpr(comp_op, rhs))
        if not rhses:
            return lhs
        return ComparisonExprChain(lhs, rhses)

    # This signature is necessarily imprecise, but could be better with
    # more typing primitives.
    def precedence_parse(self, pkl: InnerPeekable, operators: List[List[Tuple[Callable[..., Token], Callable[[Any, Any, Any], Tree]]]], atom: Callable[[InnerPeekable], Union[Token, Tree]], idx=0) -> Union[Token, Tree]:
        # A precedence parser does less work because most expressions don't
        # nest at every level.
        # TODO: Unfortunately, this isn't one yet.
        # But at least it fills in pkl.expected ...
        if idx == len(operators):
            return atom(pkl)
        lhs = self.precedence_parse(pkl, operators, atom, idx + 1)
        while True:
            for op_cls, tree_cls in operators[idx]:
                op = pkl.pop_if(op_cls)
                if op:
                    rhs = self.precedence_parse(pkl, operators, atom, idx + 1)
                    lhs = tree_cls(lhs, op, rhs)
                    break
            else:
                break
        return lhs

    def parse_or_expr(self, pkl: InnerPeekable) -> AnyOrExpr:
        operators = [
                [(OpVBar, OrExpr)],
                [(OpCircumflex, XorExpr)],
                [(OpAmper, AndExpr)],
                [(OpLShift, LShiftExpr), (OpRShift, RShiftExpr)],
                [(OpPlus, PlusExpr), (OpMinus, MinusExpr)],
                [(OpStar, StarTerm), (OpClassicSlash, ClassicSlashTerm), (OpSlash, SlashTerm), (OpPercent, PercentTerm), (OpSlashSlash, SlashSlashTerm)],
        ] # type: List[List[Tuple[Callable[..., Token], Callable[[Any, Any, Any], Tree]]]]
        if self.dialect.matmul:
            operators[-1].append((OpAt, AtTerm))
        atom = self.parse_factor
        return self.precedence_parse(pkl, operators, atom)

    def parse_factor(self, pkl: InnerPeekable) -> AnyFactor:
        if pkl.peek_if(OpPlus):
            return PlusFactor(pkl.expect(OpPlus), self.parse_factor(pkl))
        if pkl.peek_if(OpMinus):
            return MinusFactor(pkl.expect(OpMinus), self.parse_factor(pkl))
        if pkl.peek_if(OpTilde):
            return TildeFactor(pkl.expect(OpTilde), self.parse_factor(pkl))
        return self.parse_power(pkl)

    def parse_power(self, pkl: InnerPeekable) -> AnyPower:
        lhs = self.parse_await_expr(pkl)
        op = pkl.pop_if(OpStarStar)
        if op:
            rhs = self.parse_factor(pkl)
            return Power(lhs, op, rhs)
        return lhs

    def parse_await_expr(self, pkl: InnerPeekable) -> AnyAwaitExpr:
        kw_await = pkl.pop_if(KwAwait)
        rhs = self.parse_postfix_expr(pkl)
        if kw_await:
            return AwaitExpr(kw_await, rhs)
        return rhs

    def parse_postfix_expr(self, pkl: InnerPeekable) -> AnyPostfixExpr:
        lhs = self.parse_atom(pkl) # type: AnyPostfixExpr
        while True:
            dot = pkl.pop_if(OpDot)
            if dot:
                name = pkl.ensure(TokName, self.cm)
                lhs = AttributeRef(lhs, dot, name)
                continue
            if pkl.peek_if(OpLPar):
                open1, pkt = pkl.enter_token_tree(OpLPar)
                if not pkt.peek_if(OpRPar):
                    args = self.parse_arg_list(pkt)
                else:
                    args = None
                close1 = pkt.ensure(OpRPar, self.cm)
                lhs = CallExpr(lhs, open1, args, close1)
                continue
            if pkl.peek_if(OpLSqb):
                open2, pkt = pkl.enter_token_tree(OpLSqb)
                subscripts = self.parse_subscript_list(pkt)
                close2 = pkt.ensure(OpRSqb, self.cm)
                lhs = Subscription(lhs, open2, subscripts, close2)
                continue
            break
        return lhs

    def parse_atom(self, pkl: InnerPeekable) -> AnyAtom:
        atom = self.really_parse_atom(pkl)
        return maybe_recover(atom)

    def really_parse_atom(self, pkl: InnerPeekable) -> AnyAtom:
        if pkl.peek_if(Union[KwNone, KwTrue, KwFalse, KwDebug, OpDotDotDot, LitInteger, LitFloat, LitImaginary, TokName]):
            return cast(AnyAtom, pkl.pop())
        if pkl.peek_if(AnyLitString):
            return self.parse_lit_strings(pkl)
        if pkl.peek_if(OpBacktick):
            open0 = pkl.expect(OpBacktick)
            tests0 = self.parse_test_list(pkl)
            close0 = pkl.ensure(OpBacktick, self.cm)
            return LegacyRepr(open0, tests0, close0)
        if pkl.peek_if(OpLPar):
            open1, pkl = pkl.enter_token_tree(OpLPar)
            if pkl.peek_if(KwYield):
                yield_expr = self.parse_yield_expr(pkl)
                close1 = pkl.ensure(OpRPar, self.cm)
                return YieldAtom(open1, yield_expr, close1)
            items1 = [] # type: List[SequenceItemAndOpComma]
            if not pkl.peek_if(Union[OpStar, OpRPar]):
                test1 = self.parse_test(pkl)
                if pkl.peek_if(KwFor):
                    comp_for1 = self.parse_comp_for(pkl)
                    close1 = pkl.ensure(OpRPar, self.cm)
                    return GeneratorExpr(open1, test1, comp_for1, close1)
                if pkl.peek_if(OpRPar):
                    close1 = pkl.ensure(OpRPar, self.cm)
                    return ParenthForm(open1, test1, close1)
                comma1 = pkl.ensure(OpComma, self.cm)
                items1.append(SequenceItemAndOpComma(test1, comma1))
            while not pkl.peek_if(OpRPar):
                star1 = pkl.pop_if(OpStar)
                test1 = self.parse_test(pkl)
                star_test1 = StarTest(star1, test1) if star1 else test1
                comma1 = pkl.pop_if(OpComma)
                items1.append(SequenceItemAndOpComma(star_test1, comma1))
                if not comma1:
                    break
            close1 = pkl.ensure(OpRPar, self.cm)
            if not items1:
                return ParenthForm(open1, None, close1)
            return ParenthForm(open1, CommaSequenceItem(items1), close1)
        if pkl.peek_if(OpLSqb):
            open2, pkl = pkl.enter_token_tree(OpLSqb)
            items2 = [] # type: List[SequenceItemAndOpComma]
            comma2 = cast(OpComma, object())
            if not pkl.peek_if(Union[OpStar, OpRSqb]):
                test2 = self.parse_test(pkl)
                if pkl.peek_if(KwFor):
                    list_for2 = self.parse_list_for(pkl)
                    close2 = pkl.ensure(OpRSqb, self.cm)
                    return ListComprehension(open2, test2, list_for2, close2)
                comma2 = pkl.pop_if(OpComma)
                items2.append(SequenceItemAndOpComma(test2, comma2))
            while comma2 and not pkl.peek_if(OpRSqb):
                star2 = pkl.pop_if(OpStar)
                test2 = self.parse_test(pkl)
                star_test2 = StarTest(star2, test2) if star2 else test2
                comma2 = pkl.pop_if(OpComma)
                items2.append(SequenceItemAndOpComma(star_test2, comma2))
            close2 = pkl.ensure(OpRSqb, self.cm)
            return ListLiteral(open2, items2, close2)
        if pkl.peek_if(OpLBrace):
            open3, pkl = pkl.enter_token_tree(OpLBrace)
            items3map = [] # type: List[MappingItemAndOpComma]
            items3set = [] # type: List[SequenceItemAndOpComma]
            comma3 = cast(OpComma, object())
            if not pkl.peek_if(Union[OpStarStar, OpRBrace]):
                if not pkl.peek_if(OpStar):
                    key3 = self.parse_test(pkl)
                    colon3 = pkl.pop_if(OpColon)
                    if colon3:
                        value3 = self.parse_test(pkl)
                        item3map1 = TestColonTest(key3, colon3, value3)
                        if pkl.peek_if(KwFor):
                            comp_for3 = self.parse_comp_for(pkl)
                            close3 = pkl.ensure(OpRBrace, self.cm)
                            return DictComprehension(open3, item3map1, comp_for3, close3)
                        comma3 = pkl.pop_if(OpComma)
                        items3map.append(MappingItemAndOpComma(item3map1, comma3))
                    elif pkl.peek_if(KwFor):
                        comp_for3 = self.parse_comp_for(pkl)
                        close3 = pkl.ensure(OpRBrace, self.cm)
                        return SetComprehension(open3, key3, comp_for3, close3)
                    else:
                        comma3 = pkl.pop_if(OpComma)
                        items3set.append(SequenceItemAndOpComma(key3, comma3))
                if not items3map:
                    while comma3 and not pkl.peek_if(OpRBrace):
                        if pkl.peek_if(OpStar):
                            star3 = pkl.expect(OpStar)
                            key3 = self.parse_test(pkl)
                            item3set = StarTest(star3, key3) # type: SequenceItem
                        else:
                            item3set = key3 = self.parse_test(pkl)
                        comma3 = pkl.pop_if(OpComma)
                        items3set.append(SequenceItemAndOpComma(item3set, comma3))
                    close3 = pkl.ensure(OpRBrace, self.cm)
                    return SetLiteral(open3, items3set, close3)
            while comma3 and not pkl.peek_if(OpRBrace):
                if pkl.peek_if(OpStarStar):
                    star_star3 = pkl.expect(OpStarStar)
                    key3 = self.parse_test(pkl)
                    item3map = StarStarTest(star_star3, key3) # type: MappingItem
                else:
                    key3 = self.parse_test(pkl)
                    colon3 = pkl.ensure(OpColon, self.cm)
                    value3 = self.parse_test(pkl)
                    item3map = TestColonTest(key3, colon3, value3)
                comma3 = pkl.pop_if(OpComma)
                items3map.append(MappingItemAndOpComma(item3map, comma3))
            close3 = pkl.ensure(OpRBrace, self.cm)
            return DictLiteral(open3, items3map, close3)
        return make_error(cast(Union[Token, TokenTree], pkl.pop()))

    def parse_lit_strings(self, pkl: InnerPeekable) -> LitStrings:
        strings = [] # type: List[AnyLitString]
        first = pkl.expect(AnyLitString)
        strings.append(first)
        while pkl.peek_if(AnyLitString):
            s = pkl.expect(AnyLitString)
            strings.append(s)
        rv = LitStrings(strings)
        if not self.dialect.mixed_strings:
            for x in strings:
                if type(x) is not type(strings[0]):
                    self.cm.error(x._span, 'mixed bytes and unicode strings not allowed')
                    rv = cast(LitStrings, recover(rv))
                    break
        return rv

    def parse_subscript_list(self, pkl: InnerPeekable) -> SubscriptList:
        items = [] # type: List[SubscriptAndOpComma]
        subscript = self.parse_subscript(pkl)
        if pkl.peek_if(OpRSqb):
            return subscript
        comma = pkl.ensure(OpComma, self.cm)
        items.append(SubscriptAndOpComma(subscript, comma))
        while not pkl.peek_if(OpRSqb):
            subscript = self.parse_subscript(pkl)
            comma = pkl.pop_if(OpComma)
            items.append(SubscriptAndOpComma(subscript, comma))
            if not comma:
                break
        return CommaSubscript(items)

    def parse_subscript(self, pkl: InnerPeekable) -> Subscript:
        if not pkl.peek_if(OpColon):
            start = self.parse_test(pkl)
        else:
            start = None
        if not pkl.peek_if(OpColon):
            return start
        if not pkl.peek_if(Union[OpComma, OpRSqb]):
            colon1 = pkl.ensure(OpColon, self.cm)
        else:
            colon1 = None
        if not pkl.peek_if(Union[OpColon, OpComma, OpRSqb]):
            stop = self.parse_test(pkl)
        else:
            stop = None
        if not pkl.peek_if(Union[OpComma, OpRSqb]):
            colon2 = pkl.ensure(OpColon, self.cm)
        else:
            colon2 = None
        if not pkl.peek_if(Union[OpComma, OpRSqb]):
            step = self.parse_test(pkl)
        else:
            step = None
        return ProperSlice(start, colon1, stop, colon2, step)

    def parse_list_for(self, pkl: InnerPeekable) -> ListFor:
        kw_for = pkl.expect(KwFor)
        targets = self.parse_target_list(pkl)
        kw_in = pkl.ensure(KwIn, self.cm)
        tests = self.parse_test_list_nocond(pkl)
        comp_iter = self.parse_opt_list_iter(pkl)
        return ListFor(kw_for, targets, kw_in, tests, comp_iter)

    def parse_comp_for(self, pkl: InnerPeekable) -> CompFor:
        kw_for = pkl.expect(KwFor)
        targets = self.parse_target_list(pkl)
        kw_in = pkl.ensure(KwIn, self.cm)
        test = self.parse_or_test(pkl)
        if pkl.peek_if(KwFor):
            comp_iter = self.parse_comp_for(pkl) # type: Optional[CompIter]
        elif pkl.peek_if(KwIf):
            comp_iter = self.parse_comp_if(pkl)
        else:
            comp_iter = None
        return CompFor(kw_for, targets, kw_in, test, comp_iter)

    def parse_opt_list_iter(self, pkl: InnerPeekable) -> Optional[ListIter]:
        if pkl.peek_if(KwFor):
            return self.parse_list_for(pkl)
        elif pkl.peek_if(KwIf):
            return self.parse_list_if(pkl)
        else:
            return None

    def parse_opt_comp_iter(self, pkl: InnerPeekable) -> Optional[CompIter]:
        if pkl.peek_if(KwFor):
            return self.parse_comp_for(pkl)
        elif pkl.peek_if(KwIf):
            return self.parse_comp_if(pkl)
        else:
            return None

    def parse_list_if(self, pkl: InnerPeekable) -> ListIf:
        kw_if = pkl.expect(KwIf)
        test = self.parse_test_nocond(pkl, sloppy=True)
        comp_iter = self.parse_opt_list_iter(pkl)
        return ListIf(kw_if, test, comp_iter)

    def parse_comp_if(self, pkl: InnerPeekable) -> CompIf:
        kw_if = pkl.expect(KwIf)
        test = self.parse_test_nocond(pkl, sloppy=True)
        comp_iter = self.parse_opt_comp_iter(pkl)
        return CompIf(kw_if, test, comp_iter)

def parse(cm: CodeMap, dialect: Dialect, name: str, txt: Union[str, bytes]) -> FileInput:
    return Parser(cm, dialect).parse(name, txt)[0]


def _emit_node(cm: CodeMap, node: Union[Token, Tree, None], depth: int = 0) -> None:
    indent = ' ' * depth
    depth += 1
    if node is None:
        return
    if isinstance(node, Token):
        cm.warning(node._span, indent + 'token %s' % type(node).__name__)
        return
    tree = cast(Tree, node)
    cm.warning(tree._span, indent + 'begin tree %s' % type(node).__name__)
    for c in tree._children:
        _emit_node(cm, c, depth)
    cm.warning(tree._span, indent + 'end tree %s' % type(node).__name__)

def _do_file(cm: CodeMap, file: FileInput) -> None:
    _emit_node(cm, file, 0)

def _do_input(cm: CodeMap, dialect: Dialect, name: str, txt: bytes) -> bool:
    file = parse(cm, dialect, name, txt)
    _do_file(cm, file)
    print('Parsed file %r with %d errors' % (name, cm.errors.count))
    rv = bool(cm.errors.count)
    cm.errors.count = 0
    return rv

def main() -> None:
    import sys
    args = sys.argv[1:]
    cm = CodeMap()
    dialect = Dialect(sys.version[:3])
    rv = 0
    for a in args:
        with open(a, 'rb') as f:
            t = f.read()
        rv += _do_input(cm, dialect, a, t)
    if not args:
        t = sys.stdin.read()
        rv += _do_input(cm, dialect, '<stdin>', t)
    print('Parsed %d files; %d had errors.' % (len(cm.files.data), rv))
    sys.exit(min(rv, 125))

if __name__ == '__main__':
    main()
