import inspect
import io
import sys
import textwrap

import typing
from typing import (
        cast,
)

from mypy.myunit import Suite, assert_equal, assert_raises
from mypy.syntax.span import (
        CodeMap,
        FileErrorStream,
        ForbidErrorStream,
        Span,
)
from mypy.syntax.dialect import Dialect
from mypy.syntax.lexer import (
        Lexer,
        TokenOrTokenTree,
        TokenLineOrTokenBlock,
        TokenTree,
        TokenLine,
        TokenBlock,
        TokenFile,
)
from mypy.syntax.tokens import *


class Counter:
    """Keep a running total of values passed.

    >>> c = Counter(3)
    >>> c(0)
    3
    >>> c(1)
    4
    """

    def __init__(self, value: int = 0) -> None:
        self.value = value

    def __call__(self, delta: int) -> int:
        self.value += delta
        return self.value

def dump_token(out: typing.IO[str], token: Token) -> None:
    w = out.writelines
    for c in token._comments:
        w([c._whitespace, c._text])
    w([token._whitespace, token._text])

def dump_tree(out: typing.IO[str], token_tree: TokenOrTokenTree) -> None:
    if isinstance(token_tree, Token):
        dump_token(out, token_tree)
        return
    dump_token(out, cast(TokenTree, token_tree)._open)
    for e in cast(TokenTree, token_tree)._inner:
        dump_tree(out, e)
    dump_token(out, cast(TokenTree, token_tree)._close)

def dump_line(out: typing.IO[str], token_line: TokenLine) -> None:
    for e in token_line._contents:
        dump_tree(out, e)
    dump_token(out, token_line._terminator)

def dump_block(out: typing.IO[str], token_block: TokenLineOrTokenBlock) -> None:
    if isinstance(token_block, TokenLine):
        dump_line(out, token_block)
        return
    dump_token(out, cast(TokenBlock, token_block)._open)
    for e in cast(TokenBlock, token_block)._inner:
        dump_block(out, e)
    dump_token(out, cast(TokenBlock, token_block)._close)

def dump_file(out: typing.IO[str], token_file: TokenFile) -> None:
    for e in token_file._contents:
        dump_block(out, e)
    dump_token(out, token_file._terminator)


def make_suite(version: str) -> type:
    dialect = Dialect(version)

    class L:
        def __init__(self, txt: str) -> None:
            frame, _src_filename, _src_line, caller, _src_txt, _src_idx = inspect.stack()[1]
            cls_name = type(frame.f_locals['self']).__name__
            errors = io.StringIO()
            cm = CodeMap(FileErrorStream(errors))
            if not txt.endswith('\n'):
                txt += '\n'
            file = cm.add('<%s>' % caller, txt)

            self.lexer = Lexer(cm, file, dialect)
            self.input = txt
            self.output = io.StringIO()
            self.errors = errors

        def __enter__(self) -> 'L':
            return self

        def __exit__(self, ty, val, tb) -> bool:
            if val is None:
                assert_equal(self.input, self.output.getvalue())
                assert_equal('', self.errors.getvalue())
            return False

        def expect(self, tok: Token, err: str = '') -> None:
            err = textwrap.dedent(err.lstrip('\n'))
            assert err or not isinstance(tok, TokError)
            assert_equal(self.errors.getvalue(), '')
            t = self.lexer.pull_token()
            w = self.output.writelines
            for c in t._comments:
                w([c._whitespace, c._text])
            w([t._whitespace, t._text])
            assert_equal(t, tok)
            assert_equal(self.errors.getvalue(), err)
            self.errors.seek(0)
            self.errors.truncate()

    def cmp_file(input: str, expected_output: TokenFile, expected_errors: str = '') -> None:
        expected_errors = textwrap.dedent(expected_errors.lstrip('\n'))

        frame, _src_filename, _src_line, caller, _src_txt, _src_idx = inspect.stack()[1]
        cls_name = type(frame.f_locals['self']).__name__

        errors = io.StringIO()
        cm = CodeMap(FileErrorStream(errors))
        if not input.endswith('\n'):
            input += '\n'
        file = cm.add('<%s>' % caller, input)

        lexer = Lexer(cm, file, dialect)
        actual_output = lexer.pull_file()
        raw_output = io.StringIO()
        dump_file(raw_output, actual_output)

        assert_equal(input, raw_output.getvalue())
        assert_equal(expected_output, actual_output)
        assert_equal(expected_errors, errors.getvalue())

    class LexerSuite(Suite):

        def test_tok_eof(self):
            with L('') as lex:
                lex.expect(TokEof([], '\n', '', Span(1, 1, None)))
            with L(' ') as lex:
                lex.expect(TokEof([], ' \n', '', Span(2, 2, None)))

        def test_tok_comment(self):
            with L(' # This page intentionally left blank\n') as lex:
                lex.expect(TokEof([
                    Comment(' ', '# This page intentionally left blank', Span(1, 37, None)),
                ], '\n', '', Span(38, 38, None)))
            with L('\n # foo\n # bar\n') as lex:
                lex.expect(TokEof([
                    Comment('\n ', '# foo', Span(2, 7, None)),
                    Comment('\n ', '# bar', Span(9, 14, None)),
                ], '\n', '', Span(15, 15, None)))

        def test_tok_error(self):
            with L('$') as lex:
                lex.expect(TokError([], '', '$', Span(0, 1, None)),
                        '''
                        <test_tok_error>:1:1: error: invalid token
                        $
                        ^
                        ''')
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))

        def test_tok_indent(self):
            with L('  abc \n      def\n') as lex:
                lex.expect(TokIndent([], '', '  ', Span(0, 2, None)))
                lex.expect(TokName([], '', 'abc', Span(2, 5, None)))
                lex.expect(TokNewline([], ' ', '\n', Span(6, 7, None)))
                lex.expect(TokIndent([], '  ', '    ', Span(9, 13, None)))
                lex.expect(KwDef([], '', 'def', Span(13, 16, None)))
                lex.expect(TokNewline([], '', '\n', Span(16, 17, None)))
                lex.expect(TokDedent([], '', '', Span(17, 17, None)))
                lex.expect(TokDedent([], '', '', Span(17, 17, None)))
                lex.expect(TokEof([], '', '', Span(17, 17, None)))

        def test_tok_dedent_invalid(self):
            with L('    abc\n  def') as lex:
                lex.expect(TokIndent([], '', '    ', Span(0, 4, None)))
                lex.expect(TokName([], '', 'abc', Span(4, 7, None)))
                lex.expect(TokNewline([], '', '\n', Span(7, 8, None)))
                lex.expect(KwDef([], '  ', 'def', Span(10, 13, None)),
                        '''
                        <test_tok_dedent_invalid>:2:1: error: invalid dedent level
                          def
                        ^~
                        ''')
                lex.expect(TokNewline([], '', '\n', Span(13, 14, None)))
                lex.expect(TokDedent([], '', '', Span(14, 14, None)))
                lex.expect(TokEof([], '', '', Span(14, 14, None)))


        def test_op_tilde(self):
            with L('~') as lex:
                lex.expect(OpTilde([], '', '~', Span(0, 1, None)))
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))

        def test_op_less(self):
            with L('<') as lex:
                lex.expect(OpLess([], '', '<', Span(0, 1, None)))
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))

        def test_op_greater(self):
            with L('>') as lex:
                lex.expect(OpGreater([], '', '>', Span(0, 1, None)))
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))

        def test_op_less_equal(self):
            with L('<=') as lex:
                lex.expect(OpLessEqual([], '', '<=', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))

        def test_op_greater_equal(self):
            with L('>=') as lex:
                lex.expect(OpGreaterEqual([], '', '>=', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))

        def test_op_eq_equal(self):
            with L('==') as lex:
                lex.expect(OpEqEqual([], '', '==', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))

        def test_op_not_equal(self):
            with L('!=') as lex:
                lex.expect(OpNotEqual([], '', '!=', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))
            if dialect.ne_angle:
                with L('<>') as lex:
                    lex.expect(OpNotEqual([], '', '<>', Span(0, 2, None)))
                    lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                    lex.expect(TokEof([], '', '', Span(3, 3, None)))
            else:
                with L('<>') as lex:
                    lex.expect(OpLess([], '', '<', Span(0, 1, None)))
                    lex.expect(OpGreater([], '', '>', Span(1, 2, None)))
                    lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                    lex.expect(TokEof([], '', '', Span(3, 3, None)))
            if 'barry_as_FLUFL' in dialect.possible_futures:
                with L('from __future__ import barry_as_FLUFL\n!=') as lex:
                    lex.expect(KwFrom([], '', 'from', Span(0, 4, None)))
                    lex.expect(TokName([], ' ', '__future__', Span(5, 15, None)))
                    lex.expect(KwImport([], ' ', 'import', Span(16, 22, None)))
                    lex.expect(TokName([], ' ', 'barry_as_FLUFL', Span(23, 37, None)))
                    lex.expect(TokNewline([], '', '\n', Span(37, 38, None)))
                    lex.expect(TokError([], '', '!', Span(38, 39, None)),
                            '''
                            <test_op_not_equal>:2:1: error: invalid token
                            !=
                            ^
                            ''')
                    lex.expect(OpEqual([], '', '=', Span(39, 40, None)))
                    lex.expect(TokNewline([], '', '\n', Span(40, 41, None)))
                    lex.expect(TokEof([], '', '', Span(41, 41, None)))
                with L('from __future__ import barry_as_FLUFL\n<>') as lex:
                    lex.expect(KwFrom([], '', 'from', Span(0, 4, None)))
                    lex.expect(TokName([], ' ', '__future__', Span(5, 15, None)))
                    lex.expect(KwImport([], ' ', 'import', Span(16, 22, None)))
                    lex.expect(TokName([], ' ', 'barry_as_FLUFL', Span(23, 37, None)))
                    lex.expect(TokNewline([], '', '\n', Span(37, 38, None)))
                    lex.expect(OpNotEqual([], '', '<>', Span(38, 40, None)))
                    lex.expect(TokNewline([], '', '\n', Span(40, 41, None)))
                    lex.expect(TokEof([], '', '', Span(41, 41, None)))


        def test_op_plus(self):
            with L('+') as lex:
                lex.expect(OpPlus([], '', '+', Span(0, 1, None)))
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))

        def test_op_minus(self):
            with L('-') as lex:
                lex.expect(OpMinus([], '', '-', Span(0, 1, None)))
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))

        def test_op_star(self):
            with L('*') as lex:
                lex.expect(OpStar([], '', '*', Span(0, 1, None)))
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))

        def test_op_star_star(self):
            with L('**') as lex:
                lex.expect(OpStarStar([], '', '**', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))

        def test_op_slash(self):
            with L('/') as lex:
                if dialect.default_true_division:
                    err = ''
                    cls = OpSlash
                else:
                    err = '''
                    <test_op_slash>:1:1: warning: classic division; use true division or floor division instead
                    /
                    ^
                    '''
                    cls = OpClassicSlash
                lex.expect(cls([], '', '/', Span(0, 1, None)),
                        err)
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))

        def test_op_slash_slash(self):
            with L('//') as lex:
                if dialect.floor_division:
                    lex.expect(OpSlashSlash([], '', '//', Span(0, 2, None)))
                else:
                    lex.expect(OpClassicSlash([], '', '/', Span(0, 1, None)))
                    lex.expect(OpClassicSlash([], '', '/', Span(1, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))

        def test_op_percent(self):
            with L('%') as lex:
                lex.expect(OpPercent([], '', '%', Span(0, 1, None)))
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))

        def test_op_at(self):
            with L('@') as lex:
                if dialect.decorators:
                    lex.expect(OpAt([], '', '@', Span(0, 1, None)))
                else:
                    lex.expect(TokError([], '', '@', Span(0, 1, None)),
                                '''
                                <test_op_at>:1:1: error: invalid token
                                @
                                ^
                                ''')
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))

        def test_op_l_shift(self):
            with L('<<') as lex:
                lex.expect(OpLShift([], '', '<<', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))

        def test_op_r_shift(self):
            with L('>>') as lex:
                lex.expect(OpRShift([], '', '>>', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))

        def test_op_amper(self):
            with L('&') as lex:
                lex.expect(OpAmper([], '', '&', Span(0, 1, None)))
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))

        def test_op_v_bar(self):
            with L('|') as lex:
                lex.expect(OpVBar([], '', '|', Span(0, 1, None)))
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))

        def test_op_circumflex(self):
            with L('^') as lex:
                lex.expect(OpCircumflex([], '', '^', Span(0, 1, None)))
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))


        def test_op_plus_equal(self):
            with L('+=') as lex:
                lex.expect(OpPlusEqual([], '', '+=', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))

        def test_op_minus_equal(self):
            with L('-=') as lex:
                lex.expect(OpMinusEqual([], '', '-=', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))

        def test_op_star_equal(self):
            with L('*=') as lex:
                lex.expect(OpStarEqual([], '', '*=', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))

        def test_op_star_star_equal(self):
            with L('**=') as lex:
                lex.expect(OpStarStarEqual([], '', '**=', Span(0, 3, None)))
                lex.expect(TokNewline([], '', '\n', Span(3, 4, None)))
                lex.expect(TokEof([], '', '', Span(4, 4, None)))

        def test_op_slash_equal(self):
            with L('/=') as lex:
                if dialect.default_true_division:
                    err = ''
                    cls = OpSlashEqual
                else:
                    err = '''
                    <test_op_slash_equal>:1:1: warning: classic division; use true division or floor division instead
                    /=
                    ^~
                    '''
                    cls = OpClassicSlashEqual
                lex.expect(cls([], '', '/=', Span(0, 2, None)),
                        err)
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))

        def test_op_slash_slash_equal(self):
            with L('//=') as lex:
                if dialect.floor_division:
                    lex.expect(OpSlashSlashEqual([], '', '//=', Span(0, 3, None)))
                else:
                    lex.expect(OpClassicSlash([], '', '/', Span(0, 1, None)))
                    lex.expect(OpClassicSlashEqual([], '', '/=', Span(1, 3, None)))
                lex.expect(TokNewline([], '', '\n', Span(3, 4, None)))
                lex.expect(TokEof([], '', '', Span(4, 4, None)))

        def test_op_percent_equal(self):
            with L('%=') as lex:
                lex.expect(OpPercentEqual([], '', '%=', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))

        def test_op_at_equal(self):
            if dialect.matmul:
                with L('@=') as lex:
                    lex.expect(OpAtEqual([], '', '@=', Span(0, 2, None)))
                    lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                    lex.expect(TokEof([], '', '', Span(3, 3, None)))
            else:
                with L('@=') as lex:
                    if dialect.decorators:
                        lex.expect(OpAt([], '', '@', Span(0, 1, None)))
                    else:
                        lex.expect(TokError([], '', '@', Span(0, 1, None)),
                                '''
                                <test_op_at_equal>:1:1: error: invalid token
                                @=
                                ^
                                ''')
                    lex.expect(OpEqual([], '', '=', Span(1, 2, None)))
                    lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                    lex.expect(TokEof([], '', '', Span(3, 3, None)))

        def test_op_l_shift_equal(self):
            with L('<<=') as lex:
                lex.expect(OpLShiftEqual([], '', '<<=', Span(0, 3, None)))
                lex.expect(TokNewline([], '', '\n', Span(3, 4, None)))
                lex.expect(TokEof([], '', '', Span(4, 4, None)))

        def test_op_r_shift_equal(self):
            with L('>>=') as lex:
                lex.expect(OpRShiftEqual([], '', '>>=', Span(0, 3, None)))
                lex.expect(TokNewline([], '', '\n', Span(3, 4, None)))
                lex.expect(TokEof([], '', '', Span(4, 4, None)))

        def test_op_amper_equal(self):
            with L('&=') as lex:
                lex.expect(OpAmperEqual([], '', '&=', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))

        def test_op_v_bar_equal(self):
            with L('|=') as lex:
                lex.expect(OpVBarEqual([], '', '|=', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))

        def test_op_circumflex_equal(self):
            with L('^=') as lex:
                lex.expect(OpCircumflexEqual([], '', '^=', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))


        def test_op_equal(self):
            with L('=') as lex:
                lex.expect(OpEqual([], '', '=', Span(0, 1, None)))
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))

        # These 9 tests are Special™.
        def test_op_l_par(self):
            with L('(') as lex:
                lex.expect(OpLPar([], '', '(', Span(0, 1, None)))
                lex.expect(TokEof([], '\n', '', Span(2, 2, None)))

        def test_op_r_par(self):
            with L(')') as lex:
                lex.expect(OpRPar([], '', ')', Span(0, 1, None)))
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))

        def test_op_both_par(self):
            with L('()') as lex:
                lex.expect(OpLPar([], '', '(', Span(0, 1, None)))
                lex.expect(OpRPar([], '', ')', Span(1, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))

        def test_op_l_sqb(self):
            with L('[') as lex:
                lex.expect(OpLSqb([], '', '[', Span(0, 1, None)))
                lex.expect(TokEof([], '\n', '', Span(2, 2, None)))

        def test_op_r_sqb(self):
            with L(']') as lex:
                lex.expect(OpRSqb([], '', ']', Span(0, 1, None)))
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))

        def test_op_both_sqb(self):
            with L('[]') as lex:
                lex.expect(OpLSqb([], '', '[', Span(0, 1, None)))
                lex.expect(OpRSqb([], '', ']', Span(1, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))

        def test_op_l_brace(self):
            with L('{') as lex:
                lex.expect(OpLBrace([], '', '{', Span(0, 1, None)))
                lex.expect(TokEof([], '\n', '', Span(2, 2, None)))

        def test_op_r_brace(self):
            with L('}') as lex:
                lex.expect(OpRBrace([], '', '}', Span(0, 1, None)))
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))

        def test_op_both_brace(self):
            with L('{}') as lex:
                lex.expect(OpLBrace([], '', '{', Span(0, 1, None)))
                lex.expect(OpRBrace([], '', '}', Span(1, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))

        def test_op_comma(self):
            with L(',') as lex:
                lex.expect(OpComma([], '', ',', Span(0, 1, None)))
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))

        def test_op_colon(self):
            with L(':') as lex:
                lex.expect(OpColon([], '', ':', Span(0, 1, None)))
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))

        def test_op_dot(self):
            with L('.') as lex:
                lex.expect(OpDot([], '', '.', Span(0, 1, None)))
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))

        def test_op_dot_dot_dot(self):
            with L('...') as lex:
                lex.expect(OpDotDotDot([], '', '...', Span(0, 3, None)))
                lex.expect(TokNewline([], '', '\n', Span(3, 4, None)))
                lex.expect(TokEof([], '', '', Span(4, 4, None)))

        def test_op_semicolon(self):
            with L(';') as lex:
                lex.expect(OpSemicolon([], '', ';', Span(0, 1, None)))
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))

        def test_op_r_arrow(self):
            with L('->') as lex:
                lex.expect(OpRArrow([], '', '->', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))


        def test_lit_bytes(self):
            if dialect.bytes_literals:
                with L("b'foo'") as lex:
                    lex.expect(LitBytes([], '', "b'foo'", Span(0, 6, None)))
                    lex.expect(TokNewline([], '', '\n', Span(6, 7, None)))
                    lex.expect(TokEof([], '', '', Span(7, 7, None)))
                with L('b"foo"') as lex:
                    lex.expect(LitBytes([], '', 'b"foo"', Span(0, 6, None)))
                    lex.expect(TokNewline([], '', '\n', Span(6, 7, None)))
                    lex.expect(TokEof([], '', '', Span(7, 7, None)))
                with L("b'''foo'''") as lex:
                    lex.expect(LitBytes([], '', "b'''foo'''", Span(0, 10, None)))
                    lex.expect(TokNewline([], '', '\n', Span(10, 11, None)))
                    lex.expect(TokEof([], '', '', Span(11, 11, None)))
                with L('b"""foo"""') as lex:
                    lex.expect(LitBytes([], '', 'b"""foo"""', Span(0, 10, None)))
                    lex.expect(TokNewline([], '', '\n', Span(10, 11, None)))
                    lex.expect(TokEof([], '', '', Span(11, 11, None)))
                with L("br'foo'") as lex:
                    lex.expect(LitBytes([], '', "br'foo'", Span(0, 7, None)))
                    lex.expect(TokNewline([], '', '\n', Span(7, 8, None)))
                    lex.expect(TokEof([], '', '', Span(8, 8, None)))
                if dialect.rb_literals:
                    with L("rb'foo'") as lex:
                        lex.expect(LitBytes([], '', "rb'foo'", Span(0, 7, None)))
                        lex.expect(TokNewline([], '', '\n', Span(7, 8, None)))
                        lex.expect(TokEof([], '', '', Span(8, 8, None)))

        def test_lit_unicode(self):
            if dialect.unicode_literals:
                with L("u'foo'") as lex:
                    lex.expect(LitUnicode([], '', "u'foo'", Span(0, 6, None)))
                    lex.expect(TokNewline([], '', '\n', Span(6, 7, None)))
                    lex.expect(TokEof([], '', '', Span(7, 7, None)))
                with L('u"foo"') as lex:
                    lex.expect(LitUnicode([], '', 'u"foo"', Span(0, 6, None)))
                    lex.expect(TokNewline([], '', '\n', Span(6, 7, None)))
                    lex.expect(TokEof([], '', '', Span(7, 7, None)))
                with L("u'''foo'''") as lex:
                    lex.expect(LitUnicode([], '', "u'''foo'''", Span(0, 10, None)))
                    lex.expect(TokNewline([], '', '\n', Span(10, 11, None)))
                    lex.expect(TokEof([], '', '', Span(11, 11, None)))
                with L('u"""foo"""') as lex:
                    lex.expect(LitUnicode([], '', 'u"""foo"""', Span(0, 10, None)))
                    lex.expect(TokNewline([], '', '\n', Span(10, 11, None)))
                    lex.expect(TokEof([], '', '', Span(11, 11, None)))
                if dialect.ur_literals:
                    with L("ur'foo'") as lex:
                        lex.expect(LitUnicode([], '', "ur'foo'", Span(0, 7, None)))
                        lex.expect(TokNewline([], '', '\n', Span(7, 8, None)))
                        lex.expect(TokEof([], '', '', Span(8, 8, None)))

        def test_lit_str(self):
            if 'unicode_literals' in dialect.possible_futures:
                with L("from __future__ import unicode_literals; 'foo'") as lex:
                    lex.expect(KwFrom([], '', 'from', Span(0, 4, None)))
                    lex.expect(TokName([], ' ', '__future__', Span(5, 15, None)))
                    lex.expect(KwImport([], ' ', 'import', Span(16, 22, None)))
                    lex.expect(TokName([], ' ', 'unicode_literals', Span(23, 39, None)))
                    lex.expect(OpSemicolon([], '', ';', Span(39, 40, None)))
                    lex.expect(LitUnicode([], ' ', "'foo'", Span(41, 46, None)))
                    lex.expect(TokNewline([], '', '\n', Span(46, 47, None)))
                    lex.expect(TokEof([], '', '', Span(47, 47, None)))
            cls = LitUnicode if dialect.default_unicode_literals else LitBytes
            with L("'foo'") as lex:
                lex.expect(cls([], '', "'foo'", Span(0, 5, None)))
                lex.expect(TokNewline([], '', '\n', Span(5, 6, None)))
                lex.expect(TokEof([], '', '', Span(6, 6, None)))
            with L("'''\nfoo\n'''") as lex:
                lex.expect(cls([], '', "'''\nfoo\n'''", Span(0, 11, None)))
                lex.expect(TokNewline([], '', '\n', Span(11, 12, None)))
                lex.expect(TokEof([], '', '', Span(12, 12, None)))

        def test_lit_int(self):
            with L('0') as lex:
                lex.expect(LitInteger([], '', '0', Span(0, 1, None)))
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))
            with L('00') as lex:
                lex.expect(LitInteger([], '', '00', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))
            with L('9') as lex:
                lex.expect(LitInteger([], '', '9', Span(0, 1, None)))
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))
            with L('19') as lex:
                lex.expect(LitInteger([], '', '19', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))
            with L('199') as lex:
                lex.expect(LitInteger([], '', '199', Span(0, 3, None)))
                lex.expect(TokNewline([], '', '\n', Span(3, 4, None)))
                lex.expect(TokEof([], '', '', Span(4, 4, None)))
            if dialect.octal_implicit:
                with L('077') as lex:
                    lex.expect(LitInteger([], '', '077', Span(0, 3, None)))
                    lex.expect(TokNewline([], '', '\n', Span(3, 4, None)))
                    lex.expect(TokEof([], '', '', Span(4, 4, None)))
            if dialect.octal_explicit:
                with L('0o77') as lex:
                    lex.expect(LitInteger([], '', '0o77', Span(0, 4, None)))
                    lex.expect(TokNewline([], '', '\n', Span(4, 5, None)))
                    lex.expect(TokEof([], '', '', Span(5, 5, None)))
                with L('0O00') as lex:
                    lex.expect(LitInteger([], '', '0O00', Span(0, 4, None)))
                    lex.expect(TokNewline([], '', '\n', Span(4, 5, None)))
                    lex.expect(TokEof([], '', '', Span(5, 5, None)))
            with L('0xfF') as lex:
                lex.expect(LitInteger([], '', '0xfF', Span(0, 4, None)))
                lex.expect(TokNewline([], '', '\n', Span(4, 5, None)))
                lex.expect(TokEof([], '', '', Span(5, 5, None)))
            with L('0X00') as lex:
                lex.expect(LitInteger([], '', '0X00', Span(0, 4, None)))
                lex.expect(TokNewline([], '', '\n', Span(4, 5, None)))
                lex.expect(TokEof([], '', '', Span(5, 5, None)))
            if dialect.binary_literals:
                with L('0b01') as lex:
                    lex.expect(LitInteger([], '', '0b01', Span(0, 4, None)))
                    lex.expect(TokNewline([], '', '\n', Span(4, 5, None)))
                    lex.expect(TokEof([], '', '', Span(5, 5, None)))
                with L('0B10') as lex:
                    lex.expect(LitInteger([], '', '0B10', Span(0, 4, None)))
                    lex.expect(TokNewline([], '', '\n', Span(4, 5, None)))
                    lex.expect(TokEof([], '', '', Span(5, 5, None)))

        def test_lit_int_bad(self):
            if not dialect.octal_implicit:
                with L('07') as lex:
                    lex.expect(TokError([], '', '0', Span(0, 1, None)),
                            '''
                            <test_lit_int_bad>:1:1: error: invalid token
                            07
                            ^
                            ''')
                    lex.expect(LitInteger([], '', '7', Span(1, 2, None)))
                    lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                    lex.expect(TokEof([], '', '', Span(3, 3, None)))
            with L('09') as lex:
                lex.expect(TokError([], '', '0', Span(0, 1, None)),
                        '''
                        <test_lit_int_bad>:1:1: error: invalid token
                        09
                        ^
                        ''')
                lex.expect(LitInteger([], '', '9', Span(1, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))
            with L('09k') as lex:
                lex.expect(TokError([], '', '0', Span(0, 1, None)),
                        '''
                        <test_lit_int_bad>:1:1: error: invalid token
                        09k
                        ^
                        ''')
                lex.expect(TokError([], '', '9', Span(1, 2, None)),
                        '''
                        <test_lit_int_bad>:1:2: error: invalid token
                        09k
                         ^
                        ''')
                lex.expect(TokName([], '', 'k', Span(2, 3, None)))
                lex.expect(TokNewline([], '', '\n', Span(3, 4, None)))
                lex.expect(TokEof([], '', '', Span(4, 4, None)))
            if dialect.octal_explicit:
                with L('0o9') as lex:
                    lex.expect(TokError([], '', '0', Span(0, 1, None)),
                            '''
                            <test_lit_int_bad>:1:1: error: invalid token
                            0o9
                            ^
                            ''')
                    lex.expect(TokName([], '', 'o9', Span(1, 3, None)))
                    lex.expect(TokNewline([], '', '\n', Span(3, 4, None)))
                    lex.expect(TokEof([], '', '', Span(4, 4, None)))
            else:
                with L('0o7') as lex:
                    lex.expect(TokError([], '', '0', Span(0, 1, None)),
                            '''
                            <test_lit_int_bad>:1:1: error: invalid token
                            0o7
                            ^
                            ''')
                    lex.expect(TokName([], '', 'o7', Span(1, 3, None)))
                    lex.expect(TokNewline([], '', '\n', Span(3, 4, None)))
                    lex.expect(TokEof([], '', '', Span(4, 4, None)))
            with L('0K') as lex:
                lex.expect(TokError([], '', '0', Span(0, 1, None)),
                        '''
                        <test_lit_int_bad>:1:1: error: invalid token
                        0K
                        ^
                        ''')
                lex.expect(TokName([], '', 'K', Span(1, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))
            with L('0xg') as lex:
                lex.expect(TokError([], '', '0', Span(0, 1, None)),
                        '''
                        <test_lit_int_bad>:1:1: error: invalid token
                        0xg
                        ^
                        ''')
                lex.expect(TokName([], '', 'xg', Span(1, 3, None)))
                lex.expect(TokNewline([], '', '\n', Span(3, 4, None)))
                lex.expect(TokEof([], '', '', Span(4, 4, None)))
            with L('0b2') as lex:
                lex.expect(TokError([], '', '0', Span(0, 1, None)),
                        '''
                        <test_lit_int_bad>:1:1: error: invalid token
                        0b2
                        ^
                        ''')
                lex.expect(TokName([], '', 'b2', Span(1, 3, None)))
                lex.expect(TokNewline([], '', '\n', Span(3, 4, None)))
                lex.expect(TokEof([], '', '', Span(4, 4, None)))

        def test_lit_long(self):
            if dialect.long_integers:
                with L('1l') as lex:
                    lex.expect(LitInteger([], '', '1l', Span(0, 2, None)))
                    lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                    lex.expect(TokEof([], '', '', Span(3, 3, None)))
                with L('0L') as lex:
                    lex.expect(LitInteger([], '', '0L', Span(0, 2, None)))
                    lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                    lex.expect(TokEof([], '', '', Span(3, 3, None)))
        def test_lit_long_bad(self):
            if dialect.long_integers:
                with L('1ll') as lex:
                    lex.expect(TokError([], '', '1', Span(0, 1, None)),
                            '''
                            <test_lit_long_bad>:1:1: error: invalid token
                            1ll
                            ^
                            ''')
                    lex.expect(TokName([], '', 'll', Span(1, 3, None)))
                    lex.expect(TokNewline([], '', '\n', Span(3, 4, None)))
                    lex.expect(TokEof([], '', '', Span(4, 4, None)))
                with L('0LL') as lex:
                    lex.expect(TokError([], '', '0', Span(0, 1, None)),
                            '''
                            <test_lit_long_bad>:1:1: error: invalid token
                            0LL
                            ^
                            ''')
                    lex.expect(TokName([], '', 'LL', Span(1, 3, None)))
                    lex.expect(TokNewline([], '', '\n', Span(3, 4, None)))
                    lex.expect(TokEof([], '', '', Span(4, 4, None)))
            else:
                with L('1l') as lex:
                    lex.expect(TokError([], '', '1', Span(0, 1, None)),
                            '''
                            <test_lit_long_bad>:1:1: error: invalid token
                            1l
                            ^
                            ''')
                    lex.expect(TokName([], '', 'l', Span(1, 2, None)))
                    lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                    lex.expect(TokEof([], '', '', Span(3, 3, None)))
                with L('0L') as lex:
                    lex.expect(TokError([], '', '0', Span(0, 1, None)),
                            '''
                            <test_lit_long_bad>:1:1: error: invalid token
                            0L
                            ^
                            ''')
                    lex.expect(TokName([], '', 'L', Span(1, 2, None)))
                    lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                    lex.expect(TokEof([], '', '', Span(3, 3, None)))

        def test_lit_float(self):
            with L('.09') as lex:
                lex.expect(LitFloat([], '', '.09', Span(0, 3, None)))
                lex.expect(TokNewline([], '', '\n', Span(3, 4, None)))
                lex.expect(TokEof([], '', '', Span(4, 4, None)))
            with L('09.09') as lex:
                lex.expect(LitFloat([], '', '09.09', Span(0, 5, None)))
                lex.expect(TokNewline([], '', '\n', Span(5, 6, None)))
                lex.expect(TokEof([], '', '', Span(6, 6, None)))
            with L('09.') as lex:
                lex.expect(LitFloat([], '', '09.', Span(0, 3, None)))
                lex.expect(TokNewline([], '', '\n', Span(3, 4, None)))
                lex.expect(TokEof([], '', '', Span(4, 4, None)))
            with L('09e09') as lex:
                lex.expect(LitFloat([], '', '09e09', Span(0, 5, None)))
                lex.expect(TokNewline([], '', '\n', Span(5, 6, None)))
                lex.expect(TokEof([], '', '', Span(6, 6, None)))
            with L('.09e09') as lex:
                lex.expect(LitFloat([], '', '.09e09', Span(0, 6, None)))
                lex.expect(TokNewline([], '', '\n', Span(6, 7, None)))
                lex.expect(TokEof([], '', '', Span(7, 7, None)))
            with L('09.09e09') as lex:
                lex.expect(LitFloat([], '', '09.09e09', Span(0, 8, None)))
                lex.expect(TokNewline([], '', '\n', Span(8, 9, None)))
                lex.expect(TokEof([], '', '', Span(9, 9, None)))
            with L('09.e09') as lex:
                lex.expect(LitFloat([], '', '09.e09', Span(0, 6, None)))
                lex.expect(TokNewline([], '', '\n', Span(6, 7, None)))
                lex.expect(TokEof([], '', '', Span(7, 7, None)))
            with L('09.E+09') as lex:
                lex.expect(LitFloat([], '', '09.E+09', Span(0, 7, None)))
                lex.expect(TokNewline([], '', '\n', Span(7, 8, None)))
                lex.expect(TokEof([], '', '', Span(8, 8, None)))
            with L('09.E-09') as lex:
                lex.expect(LitFloat([], '', '09.E-09', Span(0, 7, None)))
                lex.expect(TokNewline([], '', '\n', Span(7, 8, None)))
                lex.expect(TokEof([], '', '', Span(8, 8, None)))

        def test_lit_imaginary(self):
            with L('.09j') as lex:
                lex.expect(LitImaginary([], '', '.09j', Span(0, 4, None)))
                lex.expect(TokNewline([], '', '\n', Span(4, 5, None)))
                lex.expect(TokEof([], '', '', Span(5, 5, None)))
            with L('09.09j') as lex:
                lex.expect(LitImaginary([], '', '09.09j', Span(0, 6, None)))
                lex.expect(TokNewline([], '', '\n', Span(6, 7, None)))
                lex.expect(TokEof([], '', '', Span(7, 7, None)))
            with L('09.j') as lex:
                lex.expect(LitImaginary([], '', '09.j', Span(0, 4, None)))
                lex.expect(TokNewline([], '', '\n', Span(4, 5, None)))
                lex.expect(TokEof([], '', '', Span(5, 5, None)))
            with L('09e09j') as lex:
                lex.expect(LitImaginary([], '', '09e09j', Span(0, 6, None)))
                lex.expect(TokNewline([], '', '\n', Span(6, 7, None)))
                lex.expect(TokEof([], '', '', Span(7, 7, None)))
            with L('.09e09j') as lex:
                lex.expect(LitImaginary([], '', '.09e09j', Span(0, 7, None)))
                lex.expect(TokNewline([], '', '\n', Span(7, 8, None)))
                lex.expect(TokEof([], '', '', Span(8, 8, None)))
            with L('09.09e09j') as lex:
                lex.expect(LitImaginary([], '', '09.09e09j', Span(0, 9, None)))
                lex.expect(TokNewline([], '', '\n', Span(9, 10, None)))
                lex.expect(TokEof([], '', '', Span(10, 10, None)))
            with L('09.e09j') as lex:
                lex.expect(LitImaginary([], '', '09.e09j', Span(0, 7, None)))
                lex.expect(TokNewline([], '', '\n', Span(7, 8, None)))
                lex.expect(TokEof([], '', '', Span(8, 8, None)))
            with L('09j') as lex:
                lex.expect(LitImaginary([], '', '09j', Span(0, 3, None)))
                lex.expect(TokNewline([], '', '\n', Span(3, 4, None)))
                lex.expect(TokEof([], '', '', Span(4, 4, None)))
            with L('09.E+09J') as lex:
                lex.expect(LitImaginary([], '', '09.E+09J', Span(0, 8, None)))
                lex.expect(TokNewline([], '', '\n', Span(8, 9, None)))
                lex.expect(TokEof([], '', '', Span(9, 9, None)))
            with L('09.E-09J') as lex:
                lex.expect(LitImaginary([], '', '09.E-09J', Span(0, 8, None)))
                lex.expect(TokNewline([], '', '\n', Span(8, 9, None)))
                lex.expect(TokEof([], '', '', Span(9, 9, None)))

        def test_tok_identifier(self):
            with L('a') as lex:
                lex.expect(TokName([], '', 'a', Span(0, 1, None)))
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))
            with L('A') as lex:
                lex.expect(TokName([], '', 'A', Span(0, 1, None)))
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))
            with L('az') as lex:
                lex.expect(TokName([], '', 'az', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))
            with L('aZ') as lex:
                lex.expect(TokName([], '', 'aZ', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))
            with L('a_') as lex:
                lex.expect(TokName([], '', 'a_', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))
            with L('a9') as lex:
                lex.expect(TokName([], '', 'a9', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))
            with L('Az') as lex:
                lex.expect(TokName([], '', 'Az', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))
            with L('AZ') as lex:
                lex.expect(TokName([], '', 'AZ', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))
            with L('A_') as lex:
                lex.expect(TokName([], '', 'A_', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))
            with L('A9') as lex:
                lex.expect(TokName([], '', 'A9', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))
            with L('_9') as lex:
                lex.expect(TokName([], '', '_9', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))
            with L('_z') as lex:
                lex.expect(TokName([], '', '_z', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))
            with L('_Z') as lex:
                lex.expect(TokName([], '', '_Z', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))

        def test_identifier_under_only(self):
            # The defaulted second argument means we expect no warnings.
            # These do not ever count as dunder names.
            with L('_') as lex:
                lex.expect(TokName([], '', '_', Span(0, 1, None)))
                lex.expect(TokNewline([], '', '\n', Span(1, 2, None)))
                lex.expect(TokEof([], '', '', Span(2, 2, None)))
            with L('__') as lex:
                lex.expect(TokName([], '', '__', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))
            with L('___') as lex:
                lex.expect(TokName([], '', '___', Span(0, 3, None)))
                lex.expect(TokNewline([], '', '\n', Span(3, 4, None)))
                lex.expect(TokEof([], '', '', Span(4, 4, None)))
            with L('____') as lex:
                lex.expect(TokName([], '', '____', Span(0, 4, None)))
                lex.expect(TokNewline([], '', '\n', Span(4, 5, None)))
                lex.expect(TokEof([], '', '', Span(5, 5, None)))
            with L('_____') as lex:
                lex.expect(TokName([], '', '_____', Span(0, 5, None)))
                lex.expect(TokNewline([], '', '\n', Span(5, 6, None)))
                lex.expect(TokEof([], '', '', Span(6, 6, None)))

        def test_identifier_dunder(self):
            with L('__bogus_name__') as lex:
                lex.expect(TokName([], '', '__bogus_name__', Span(0, 14, None)),
                        '''
                        <test_identifier_dunder>:1:1: warning: unrecognized dunder name
                        __bogus_name__
                        ^~~~~~~~~~~~~~
                        ''')
                lex.expect(TokNewline([], '', '\n', Span(14, 15, None)))
                lex.expect(TokEof([], '', '', Span(15, 15, None)))

        def test_keyword_debug(self):
            with L('__debug__') as lex:
                if dialect.literal_debug:
                    lex.expect(KwDebug([], '', '__debug__', Span(0, 9, None)))
                else:
                    lex.expect(TokName([], '', '__debug__', Span(0, 9, None)))
                lex.expect(TokNewline([], '', '\n', Span(9, 10, None)))
                lex.expect(TokEof([], '', '', Span(10, 10, None)))

        def test_keyword_false(self):
            with L('False') as lex:
                if dialect.literal_bool:
                    lex.expect(KwFalse([], '', 'False', Span(0, 5, None)))
                else:
                    lex.expect(TokName([], '', 'False', Span(0, 5, None)))
                lex.expect(TokNewline([], '', '\n', Span(5, 6, None)))
                lex.expect(TokEof([], '', '', Span(6, 6, None)))

        def test_keyword_true(self):
            with L('True') as lex:
                if dialect.literal_bool:
                    lex.expect(KwTrue([], '', 'True', Span(0, 4, None)))
                else:
                    lex.expect(TokName([], '', 'True', Span(0, 4, None)))
                lex.expect(TokNewline([], '', '\n', Span(4, 5, None)))
                lex.expect(TokEof([], '', '', Span(5, 5, None)))

        def test_keyword_none(self):
            with L('None') as lex:
                if dialect.literal_none:
                    lex.expect(KwNone([], '', 'None', Span(0, 4, None)))
                else:
                    lex.expect(TokName([], '', 'None', Span(0, 4, None)))
                lex.expect(TokNewline([], '', '\n', Span(4, 5, None)))
                lex.expect(TokEof([], '', '', Span(5, 5, None)))

        def test_keyword_and(self):
            with L('and') as lex:
                lex.expect(KwAnd([], '', 'and', Span(0, 3, None)))
                lex.expect(TokNewline([], '', '\n', Span(3, 4, None)))
                lex.expect(TokEof([], '', '', Span(4, 4, None)))

        def test_keyword_as(self):
            with L('as') as lex:
                if dialect.as_keyword:
                    lex.expect(KwAs([], '', 'as', Span(0, 2, None)))
                else:
                    lex.expect(TokName([], '', 'as', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))

        def test_keyword_assert(self):
            with L('assert') as lex:
                lex.expect(KwAssert([], '', 'assert', Span(0, 6, None)))
                lex.expect(TokNewline([], '', '\n', Span(6, 7, None)))
                lex.expect(TokEof([], '', '', Span(7, 7, None)))

        def test_keyword_async(self):
            with L('async') as lex:
                if dialect.async_keyword:
                    lex.expect(KwAsync([], '', 'async', Span(0, 5, None)))
                else:
                    lex.expect(TokName([], '', 'async', Span(0, 5, None)))
                lex.expect(TokNewline([], '', '\n', Span(5, 6, None)))
                lex.expect(TokEof([], '', '', Span(6, 6, None)))
            with L('async\\\ndef') as lex:
                if dialect.coroutines:
                    lex.expect(KwAsync([], '', 'async', Span(0, 5, None)))
                else:
                    lex.expect(TokName([], '', 'async', Span(0, 5, None)))
                lex.expect(KwDef([], '\\\n', 'def', Span(7, 10, None)))
                lex.expect(TokNewline([], '', '\n', Span(10, 11, None)))
                lex.expect(TokEof([], '', '', Span(11, 11, None)))
            with L('async$') as lex:
                if dialect.async_keyword:
                    lex.expect(KwAsync([], '', 'async', Span(0, 5, None)))
                else:
                    lex.expect(TokName([], '', 'async', Span(0, 5, None)))
                lex.expect(TokError([], '', '$', Span(5, 6, None)),
                        '''
                        <test_keyword_async>:1:6: error: invalid token
                        async$
                             ^
                        ''')
                lex.expect(TokNewline([], '', '\n', Span(6, 7, None)))
                lex.expect(TokEof([], '', '', Span(7, 7, None)))
            with L('(async def)') as lex:
                lex.expect(OpLPar([], '', '(', Span(0, 1, None)))
                if dialect.coroutines:
                    lex.expect(TokError([], '', '', Span(1, 1, None)),
                            '''
                            <test_keyword_async>:1:2: error: expected ')' before 'async'
                            (async def)
                             ^~~~~
                            ''')
                    lex.expect(KwAsync([], '', 'async', Span(1, 6, None)))
                    lex.expect(KwDef([], ' ', 'def', Span(7, 10, None)))
                else:
                    lex.expect(TokName([], '', 'async', Span(1, 6, None)))
                    lex.expect(TokError([], ' ', '', Span(7, 7, None)),
                            '''
                            <test_keyword_async>:1:8: error: expected ')' before 'def'
                            (async def)
                                   ^~~
                            ''')
                    lex.expect(KwDef([], '', 'def', Span(7, 10, None)))
                lex.expect(OpRPar([], '', ')', Span(10, 11, None)))
                lex.expect(TokNewline([], '', '\n', Span(11, 12, None)))
                lex.expect(TokEof([], '', '', Span(12, 12, None)))

        def test_keyword_async_harder(self):
            with L('async await async def async await') as lex:
                if dialect.async_keyword:
                    lex.expect(KwAsync([], '', 'async', Span(0, 5, None)))
                else:
                    lex.expect(TokName([], '', 'async', Span(0, 5, None)))
                if dialect.await_keyword:
                    lex.expect(KwAwait([], ' ', 'await', Span(6, 11, None)))
                else:
                    lex.expect(TokName([], ' ', 'await', Span(6, 11, None)))
                if dialect.coroutines:
                    lex.expect(KwAsync([], ' ', 'async', Span(12, 17, None)))
                else:
                    lex.expect(TokName([], ' ', 'async', Span(12, 17, None)))
                lex.expect(KwDef([], ' ', 'def', Span(18, 21, None)))
                if dialect.coroutines:
                    lex.expect(KwAsync([], ' ', 'async', Span(22, 27, None)))
                    lex.expect(KwAwait([], ' ', 'await', Span(28, 33, None)))
                else:
                    lex.expect(TokName([], ' ', 'async', Span(22, 27, None)))
                    lex.expect(TokName([], ' ', 'await', Span(28, 33, None)))
                lex.expect(TokNewline([], '', '\n', Span(33, 34, None)))
                lex.expect(TokEof([], '', '', Span(34, 34, None)))
            if dialect.coroutines and not dialect.async_keyword:
                with L(
                        textwrap.dedent(
                            '''
                            async;await
                            def
                            async;await
                            def;async
                            async;await
                            def
                                async;await
                                def
                                async;await
                                def;async
                                async;await
                                def
                                    async;await
                                async;await
                                async def
                                async;await
                                async def;async
                                async;await
                                async def
                                    async;await
                                async;await
                            async;await
                            async def
                            async;await
                            async def;async
                            async;await
                            async def
                                async;await
                                def
                                async;await
                                def;async
                                async;await
                                def
                                    async;await
                                async;await
                                async def
                                async;await
                                async def;async
                                async;await
                                async def
                                    async;await
                                async;await
                            async;await
                            ''')) as lex:
                    rel = Counter()

                    lex.expect(TokName([], '\n', 'async', Span(rel(1), rel(5), None)))
                    lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                    lex.expect(TokName([], '', 'await', Span(rel(0), rel(5), None)))
                    lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                    lex.expect(KwDef([], '', 'def', Span(rel(0), rel(3), None)))
                    lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                    lex.expect(TokName([], '', 'async', Span(rel(0), rel(5), None)))
                    lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                    lex.expect(TokName([], '', 'await', Span(rel(0), rel(5), None)))
                    lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                    lex.expect(KwDef([], '', 'def', Span(rel(0), rel(3), None)))
                    lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                    lex.expect(TokName([], '', 'async', Span(rel(0), rel(5), None)))
                    lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                    lex.expect(TokName([], '', 'async', Span(rel(0), rel(5), None)))
                    lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                    lex.expect(TokName([], '', 'await', Span(rel(0), rel(5), None)))
                    lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                    lex.expect(KwDef([], '', 'def', Span(rel(0), rel(3), None)))
                    lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                    lex.expect(TokIndent([], '', '    ', Span(rel(0), rel(4), None)))
                    if 1:
                        lex.expect(TokName([], '', 'async', Span(rel(0), rel(5), None)))
                        lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                        lex.expect(TokName([], '', 'await', Span(rel(0), rel(5), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                        lex.expect(KwDef([], '    ', 'def', Span(rel(4), rel(3), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                        lex.expect(TokName([], '    ', 'async', Span(rel(4), rel(5), None)))
                        lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                        lex.expect(TokName([], '', 'await', Span(rel(0), rel(5), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                        lex.expect(KwDef([], '    ', 'def', Span(rel(4), rel(3), None)))
                        lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                        lex.expect(TokName([], '', 'async', Span(rel(0), rel(5), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                        lex.expect(TokName([], '    ', 'async', Span(rel(4), rel(5), None)))
                        lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                        lex.expect(TokName([], '', 'await', Span(rel(0), rel(5), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                        lex.expect(KwDef([], '    ', 'def', Span(rel(4), rel(3), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                        lex.expect(TokIndent([], '    ', '    ', Span(rel(4), rel(4), None)))
                        if 1:
                            lex.expect(TokName([], '', 'async', Span(rel(0), rel(5), None)))
                            lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                            lex.expect(TokName([], '', 'await', Span(rel(0), rel(5), None)))
                            lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))
                        lex.expect(TokDedent([], '', '', Span(rel(4), rel(0), None)))

                        lex.expect(TokName([], '    ', 'async', Span(rel(0), rel(5), None)))
                        lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                        lex.expect(TokName([], '', 'await', Span(rel(0), rel(5), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                        lex.expect(KwAsync([], '    ', 'async', Span(rel(4), rel(5), None)))
                        lex.expect(KwDef([], ' ', 'def', Span(rel(1), rel(3), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                        lex.expect(TokName([], '    ', 'async', Span(rel(4), rel(5), None)))
                        lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                        lex.expect(TokName([], '', 'await', Span(rel(0), rel(5), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                        lex.expect(KwAsync([], '    ', 'async', Span(rel(4), rel(5), None)))
                        lex.expect(KwDef([], ' ', 'def', Span(rel(1), rel(3), None)))
                        lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                        lex.expect(KwAsync([], '', 'async', Span(rel(0), rel(5), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                        lex.expect(TokName([], '    ', 'async', Span(rel(4), rel(5), None)))
                        lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                        lex.expect(TokName([], '', 'await', Span(rel(0), rel(5), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                        lex.expect(KwAsync([], '    ', 'async', Span(rel(4), rel(5), None)))
                        lex.expect(KwDef([], ' ', 'def', Span(rel(1), rel(3), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                        lex.expect(TokIndent([], '    ', '    ', Span(rel(4), rel(4), None)))
                        if 1:
                            lex.expect(KwAsync([], '', 'async', Span(rel(0), rel(5), None)))
                            lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                            lex.expect(KwAwait([], '', 'await', Span(rel(0), rel(5), None)))
                            lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))
                        lex.expect(TokDedent([], '', '', Span(rel(4), rel(0), None)))

                        lex.expect(TokName([], '    ', 'async', Span(rel(0), rel(5), None)))
                        lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                        lex.expect(TokName([], '', 'await', Span(rel(0), rel(5), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))
                    lex.expect(TokDedent([], '', '', Span(rel(0), rel(0), None)))

                    lex.expect(TokName([], '', 'async', Span(rel(0), rel(5), None)))
                    lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                    lex.expect(TokName([], '', 'await', Span(rel(0), rel(5), None)))
                    lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                    lex.expect(KwAsync([], '', 'async', Span(rel(0), rel(5), None)))
                    lex.expect(KwDef([], ' ', 'def', Span(rel(1), rel(3), None)))
                    lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                    lex.expect(TokName([], '', 'async', Span(rel(0), rel(5), None)))
                    lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                    lex.expect(TokName([], '', 'await', Span(rel(0), rel(5), None)))
                    lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                    lex.expect(KwAsync([], '', 'async', Span(rel(0), rel(5), None)))
                    lex.expect(KwDef([], ' ', 'def', Span(rel(1), rel(3), None)))
                    lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                    lex.expect(KwAsync([], '', 'async', Span(rel(0), rel(5), None)))
                    lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                    lex.expect(TokName([], '', 'async', Span(rel(0), rel(5), None)))
                    lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                    lex.expect(TokName([], '', 'await', Span(rel(0), rel(5), None)))
                    lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                    lex.expect(KwAsync([], '', 'async', Span(rel(0), rel(5), None)))
                    lex.expect(KwDef([], ' ', 'def', Span(rel(1), rel(3), None)))
                    lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                    lex.expect(TokIndent([], '', '    ', Span(rel(0), rel(4), None)))
                    if 1:
                        lex.expect(KwAsync([], '', 'async', Span(rel(0), rel(5), None)))
                        lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                        lex.expect(KwAwait([], '', 'await', Span(rel(0), rel(5), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                        lex.expect(KwDef([], '    ', 'def', Span(rel(4), rel(3), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                        lex.expect(KwAsync([], '    ', 'async', Span(rel(4), rel(5), None)))
                        lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                        lex.expect(KwAwait([], '', 'await', Span(rel(0), rel(5), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                        lex.expect(KwDef([], '    ', 'def', Span(rel(4), rel(3), None)))
                        lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                        lex.expect(TokName([], '', 'async', Span(rel(0), rel(5), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                        lex.expect(KwAsync([], '    ', 'async', Span(rel(4), rel(5), None)))
                        lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                        lex.expect(KwAwait([], '', 'await', Span(rel(0), rel(5), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                        lex.expect(KwDef([], '    ', 'def', Span(rel(4), rel(3), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                        lex.expect(TokIndent([], '    ', '    ', Span(rel(4), rel(4), None)))
                        if 1:
                            lex.expect(TokName([], '', 'async', Span(rel(0), rel(5), None)))
                            lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                            lex.expect(TokName([], '', 'await', Span(rel(0), rel(5), None)))
                            lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))
                        lex.expect(TokDedent([], '', '', Span(rel(4), rel(0), None)))

                        lex.expect(KwAsync([], '    ', 'async', Span(rel(0), rel(5), None)))
                        lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                        lex.expect(KwAwait([], '', 'await', Span(rel(0), rel(5), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                        lex.expect(KwAsync([], '    ', 'async', Span(rel(4), rel(5), None)))
                        lex.expect(KwDef([], ' ', 'def', Span(rel(1), rel(3), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                        lex.expect(KwAsync([], '    ', 'async', Span(rel(4), rel(5), None)))
                        lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                        lex.expect(KwAwait([], '', 'await', Span(rel(0), rel(5), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                        lex.expect(KwAsync([], '    ', 'async', Span(rel(4), rel(5), None)))
                        lex.expect(KwDef([], ' ', 'def', Span(rel(1), rel(3), None)))
                        lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                        lex.expect(KwAsync([], '', 'async', Span(rel(0), rel(5), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                        lex.expect(KwAsync([], '    ', 'async', Span(rel(4), rel(5), None)))
                        lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                        lex.expect(KwAwait([], '', 'await', Span(rel(0), rel(5), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                        lex.expect(KwAsync([], '    ', 'async', Span(rel(4), rel(5), None)))
                        lex.expect(KwDef([], ' ', 'def', Span(rel(1), rel(3), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                        lex.expect(TokIndent([], '    ', '    ', Span(rel(4), rel(4), None)))
                        if 1:
                            lex.expect(KwAsync([], '', 'async', Span(rel(0), rel(5), None)))
                            lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                            lex.expect(KwAwait([], '', 'await', Span(rel(0), rel(5), None)))
                            lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))
                        lex.expect(TokDedent([], '', '', Span(rel(4), rel(0), None)))

                        lex.expect(KwAsync([], '    ', 'async', Span(rel(0), rel(5), None)))
                        lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                        lex.expect(KwAwait([], '', 'await', Span(rel(0), rel(5), None)))
                        lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))
                    lex.expect(TokDedent([], '', '', Span(rel(0), rel(0), None)))

                    lex.expect(TokName([], '', 'async', Span(rel(0), rel(5), None)))
                    lex.expect(OpSemicolon([], '', ';', Span(rel(0), rel(1), None)))
                    lex.expect(TokName([], '', 'await', Span(rel(0), rel(5), None)))
                    lex.expect(TokNewline([], '', '\n', Span(rel(0), rel(1), None)))

                    lex.expect(TokEof([], '', '', Span(rel(0), rel(0), None)))

        def test_keyword_await(self):
            with L('await') as lex:
                if dialect.await_keyword:
                    lex.expect(KwAwait([], '', 'await', Span(0, 5, None)))
                else:
                    lex.expect(TokName([], '', 'await', Span(0, 5, None)))
                lex.expect(TokNewline([], '', '\n', Span(5, 6, None)))
                lex.expect(TokEof([], '', '', Span(6, 6, None)))

        def test_keyword_break(self):
            with L('break') as lex:
                lex.expect(KwBreak([], '', 'break', Span(0, 5, None)))
                lex.expect(TokNewline([], '', '\n', Span(5, 6, None)))
                lex.expect(TokEof([], '', '', Span(6, 6, None)))

        def test_keyword_class(self):
            with L('class') as lex:
                lex.expect(KwClass([], '', 'class', Span(0, 5, None)))
                lex.expect(TokNewline([], '', '\n', Span(5, 6, None)))
                lex.expect(TokEof([], '', '', Span(6, 6, None)))

        def test_keyword_continue(self):
            with L('continue') as lex:
                lex.expect(KwContinue([], '', 'continue', Span(0, 8, None)))
                lex.expect(TokNewline([], '', '\n', Span(8, 9, None)))
                lex.expect(TokEof([], '', '', Span(9, 9, None)))

        def test_keyword_def(self):
            with L('def') as lex:
                lex.expect(KwDef([], '', 'def', Span(0, 3, None)))
                lex.expect(TokNewline([], '', '\n', Span(3, 4, None)))
                lex.expect(TokEof([], '', '', Span(4, 4, None)))

        def test_keyword_del(self):
            with L('del') as lex:
                lex.expect(KwDel([], '', 'del', Span(0, 3, None)))
                lex.expect(TokNewline([], '', '\n', Span(3, 4, None)))
                lex.expect(TokEof([], '', '', Span(4, 4, None)))

        def test_keyword_elif(self):
            with L('elif') as lex:
                lex.expect(KwElif([], '', 'elif', Span(0, 4, None)))
                lex.expect(TokNewline([], '', '\n', Span(4, 5, None)))
                lex.expect(TokEof([], '', '', Span(5, 5, None)))

        def test_keyword_else(self):
            with L('else') as lex:
                lex.expect(KwElse([], '', 'else', Span(0, 4, None)))
                lex.expect(TokNewline([], '', '\n', Span(4, 5, None)))
                lex.expect(TokEof([], '', '', Span(5, 5, None)))

        def test_keyword_except(self):
            with L('except') as lex:
                lex.expect(KwExcept([], '', 'except', Span(0, 6, None)))
                lex.expect(TokNewline([], '', '\n', Span(6, 7, None)))
                lex.expect(TokEof([], '', '', Span(7, 7, None)))

        def test_keyword_exec(self):
            with L('exec') as lex:
                if dialect.exec_keyword:
                    lex.expect(KwExec([], '', 'exec', Span(0, 4, None)))
                else:
                    lex.expect(TokName([], '', 'exec', Span(0, 4, None)))
                lex.expect(TokNewline([], '', '\n', Span(4, 5, None)))
                lex.expect(TokEof([], '', '', Span(5, 5, None)))

        def test_keyword_finally(self):
            with L('finally') as lex:
                lex.expect(KwFinally([], '', 'finally', Span(0, 7, None)))
                lex.expect(TokNewline([], '', '\n', Span(7, 8, None)))
                lex.expect(TokEof([], '', '', Span(8, 8, None)))

        def test_keyword_for(self):
            with L('for') as lex:
                lex.expect(KwFor([], '', 'for', Span(0, 3, None)))
                lex.expect(TokNewline([], '', '\n', Span(3, 4, None)))
                lex.expect(TokEof([], '', '', Span(4, 4, None)))

        def test_keyword_from(self):
            with L('from') as lex:
                lex.expect(KwFrom([], '', 'from', Span(0, 4, None)))
                lex.expect(TokNewline([], '', '\n', Span(4, 5, None)))
                lex.expect(TokEof([], '', '', Span(5, 5, None)))

        def test_keyword_global(self):
            with L('global') as lex:
                lex.expect(KwGlobal([], '', 'global', Span(0, 6, None)))
                lex.expect(TokNewline([], '', '\n', Span(6, 7, None)))
                lex.expect(TokEof([], '', '', Span(7, 7, None)))

        def test_keyword_if(self):
            with L('if') as lex:
                lex.expect(KwIf([], '', 'if', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))

        def test_keyword_import(self):
            with L('import') as lex:
                lex.expect(KwImport([], '', 'import', Span(0, 6, None)))
                lex.expect(TokNewline([], '', '\n', Span(6, 7, None)))
                lex.expect(TokEof([], '', '', Span(7, 7, None)))

        def test_keyword_in(self):
            with L('in') as lex:
                lex.expect(KwIn([], '', 'in', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))

        def test_keyword_is(self):
            with L('is') as lex:
                lex.expect(KwIs([], '', 'is', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))

        def test_keyword_lambda(self):
            with L('lambda') as lex:
                lex.expect(KwLambda([], '', 'lambda', Span(0, 6, None)))
                lex.expect(TokNewline([], '', '\n', Span(6, 7, None)))
                lex.expect(TokEof([], '', '', Span(7, 7, None)))

        def test_keyword_nonlocal(self):
            with L('nonlocal') as lex:
                if dialect.nonlocal_keyword:
                    lex.expect(KwNonlocal([], '', 'nonlocal', Span(0, 8, None)))
                else:
                    lex.expect(TokName([], '', 'nonlocal', Span(0, 8, None)))
                lex.expect(TokNewline([], '', '\n', Span(8, 9, None)))
                lex.expect(TokEof([], '', '', Span(9, 9, None)))

        def test_keyword_not(self):
            with L('not') as lex:
                lex.expect(KwNot([], '', 'not', Span(0, 3, None)))
                lex.expect(TokNewline([], '', '\n', Span(3, 4, None)))
                lex.expect(TokEof([], '', '', Span(4, 4, None)))

        def test_keyword_or(self):
            with L('or') as lex:
                lex.expect(KwOr([], '', 'or', Span(0, 2, None)))
                lex.expect(TokNewline([], '', '\n', Span(2, 3, None)))
                lex.expect(TokEof([], '', '', Span(3, 3, None)))

        def test_keyword_pass(self):
            with L('pass') as lex:
                lex.expect(KwPass([], '', 'pass', Span(0, 4, None)))
                lex.expect(TokNewline([], '', '\n', Span(4, 5, None)))
                lex.expect(TokEof([], '', '', Span(5, 5, None)))

        def test_keyword_print(self):
            with L('print') as lex:
                if dialect.print_keyword:
                    lex.expect(KwPrint([], '', 'print', Span(0, 5, None)))
                else:
                    lex.expect(TokName([], '', 'print', Span(0, 5, None)))
                lex.expect(TokNewline([], '', '\n', Span(5, 6, None)))
                lex.expect(TokEof([], '', '', Span(6, 6, None)))

        def test_keyword_raise(self):
            with L('raise') as lex:
                lex.expect(KwRaise([], '', 'raise', Span(0, 5, None)))
                lex.expect(TokNewline([], '', '\n', Span(5, 6, None)))
                lex.expect(TokEof([], '', '', Span(6, 6, None)))

        def test_keyword_return(self):
            with L('return') as lex:
                lex.expect(KwReturn([], '', 'return', Span(0, 6, None)))
                lex.expect(TokNewline([], '', '\n', Span(6, 7, None)))
                lex.expect(TokEof([], '', '', Span(7, 7, None)))

        def test_keyword_try(self):
            with L('try') as lex:
                lex.expect(KwTry([], '', 'try', Span(0, 3, None)))
                lex.expect(TokNewline([], '', '\n', Span(3, 4, None)))
                lex.expect(TokEof([], '', '', Span(4, 4, None)))

        def test_keyword_while(self):
            with L('while') as lex:
                lex.expect(KwWhile([], '', 'while', Span(0, 5, None)))
                lex.expect(TokNewline([], '', '\n', Span(5, 6, None)))
                lex.expect(TokEof([], '', '', Span(6, 6, None)))

        def test_keyword_with(self):
            with L('with') as lex:
                if dialect.with_keyword:
                    lex.expect(KwWith([], '', 'with', Span(0, 4, None)))
                else:
                    lex.expect(TokName([], '', 'with', Span(0, 4, None)))
                lex.expect(TokNewline([], '', '\n', Span(4, 5, None)))
                lex.expect(TokEof([], '', '', Span(5, 5, None)))

        def test_keyword_yield(self):
            with L('yield') as lex:
                if dialect.yield_keyword:
                    lex.expect(KwYield([], '', 'yield', Span(0, 5, None)))
                else:
                    lex.expect(TokName([], '', 'yield', Span(0, 5, None)))
                lex.expect(TokNewline([], '', '\n', Span(5, 6, None)))
                lex.expect(TokEof([], '', '', Span(6, 6, None)))

        def test_error_matching(self):
            with L('{\n]}') as lex:
                lex.expect(OpLBrace([], '', '{', Span(0, 1, None)))
                lex.expect(TokError([], '\n', '', Span(2, 2, None)),
                        '''
                        <test_error_matching>:2:1: error: expected '}' before ']'
                        ]}
                        ^
                        ''')
                lex.expect(OpRSqb([], '', ']', Span(2, 3, None)))
                lex.expect(OpRBrace([], '', '}', Span(3, 4, None)))
                lex.expect(TokNewline([], '', '\n', Span(4, 5, None)))
                lex.expect(TokEof([], '', '', Span(5, 5, None)))
            with L('([{\n]})') as lex:
                lex.expect(OpLPar([], '', '(', Span(0, 1, None)))
                lex.expect(OpLSqb([], '', '[', Span(1, 2, None)))
                lex.expect(OpLBrace([], '', '{', Span(2, 3, None)))
                lex.expect(TokError([], '\n', '', Span(4, 4, None)),
                        '''
                        <test_error_matching>:2:1: error: expected '}' before ']'
                        ]})
                        ^
                        ''')
                lex.expect(OpRSqb([], '', ']', Span(4, 5, None)))
                lex.expect(TokError([], '', '', Span(5, 5, None)),
                        '''
                        <test_error_matching>:2:2: error: expected ')' before '}'
                        ]})
                         ^
                        ''')
                lex.expect(OpRBrace([], '', '}', Span(5, 6, None)))
                lex.expect(OpRPar([], '', ')', Span(6, 7, None)))
                lex.expect(TokNewline([], '', '\n', Span(7, 8, None)))
                lex.expect(TokEof([], '', '', Span(8, 8, None)))

        def test_error_matching2(self):
            cmp_file('{\n]}',
                    TokenFile(
                        [
                            TokenLine(
                                [
                                    TokenTree(
                                        OpLBrace([], '', '{', Span(0, 1, None)),
                                        [],
                                        TokError([], '\n', '', Span(2, 2, None)),
                                    ),
                                    OpRSqb([], '', ']', Span(2, 3, None)),
                                    OpRBrace([], '', '}', Span(3, 4, None)),
                                ],
                                TokNewline([], '', '\n', Span(4, 5, None)),
                            ),
                        ],
                        TokEof([], '', '', Span(5, 5, None)),
                    ),
                    '''
                    <test_error_matching2>:2:1: error: expected '}' before ']'
                    ]}
                    ^
                    <test_error_matching2>:2:1: error: unexpected ']'
                    ]}
                    ^
                    <test_error_matching2>:2:2: error: unexpected '}'
                    ]}
                     ^
                    ''')
            cmp_file('([{\n]})',
                    TokenFile(
                        [
                            TokenLine(
                                [
                                    TokenTree(
                                        OpLPar([], '', '(', Span(0, 1, None)),
                                        [
                                            TokenTree(
                                                OpLSqb([], '', '[', Span(1, 2, None)),
                                                [
                                                    TokenTree(
                                                        OpLBrace([], '', '{', Span(2, 3, None)),
                                                        [],
                                                        TokError([], '\n', '', Span(4, 4, None)),
                                                    ),
                                                ],
                                                OpRSqb([], '', ']', Span(4, 5, None)),
                                            ),
                                        ],
                                        TokError([], '', '', Span(5, 5, None)),
                                    ),
                                    OpRBrace([], '', '}', Span(5, 6, None)),
                                    OpRPar([], '', ')', Span(6, 7, None)),
                                ],
                                TokNewline([], '', '\n', Span(7, 8, None)),
                            ),
                        ],
                        TokEof([], '', '', Span(8, 8, None)),
                    ),
                    '''
                    <test_error_matching2>:2:1: error: expected '}' before ']'
                    ]})
                    ^
                    <test_error_matching2>:2:2: error: expected ')' before '}'
                    ]})
                     ^
                    <test_error_matching2>:2:2: error: unexpected '}'
                    ]})
                     ^
                    <test_error_matching2>:2:3: error: unexpected ')'
                    ]})
                      ^
                    ''')

        def test_block(self):
            #         01 234 56789 0123 456 78
            cmp_file('a\n b\n   c\n  d\n e\nf\n',
                    TokenFile(
                        [
                            TokenLine(
                                [
                                    TokName([], '', 'a', Span(0, 1, None)),
                                ],
                                TokNewline([], '', '\n', Span(1, 2, None)),
                            ),
                            TokenBlock(
                                TokIndent([], '', ' ', Span(2, 3, None)),
                                [
                                    TokenLine(
                                        [
                                            TokName([], '', 'b', Span(3, 4, None)),
                                        ],
                                        TokNewline([], '', '\n', Span(4, 5, None)),
                                    ),
                                    TokenBlock(
                                        TokIndent([], ' ', '  ', Span(6, 8, None)),
                                        [
                                            TokenLine(
                                                [
                                                    TokName([], '', 'c', Span(8, 9, None)),
                                                ],
                                                TokNewline([], '', '\n', Span(9, 10, None)),
                                            ),
                                            TokenLine(
                                                [
                                                    TokName([], '  ', 'd', Span(12, 13, None)),
                                                ],
                                                TokNewline([], '', '\n', Span(13, 14, None)),
                                            ),
                                        ],
                                        TokDedent([], '', '', Span(15, 15, None)),
                                    ),
                                    TokenLine(
                                        [
                                            TokName([], ' ', 'e', Span(15, 16, None)),
                                        ],
                                        TokNewline([], '', '\n', Span(16, 17, None)),
                                    ),
                                ],
                                TokDedent([], '', '', Span(17, 17, None)),
                            ),
                            TokenLine(
                                [
                                    TokName([], '', 'f', Span(17, 18, None)),
                                ],
                                TokNewline([], '', '\n', Span(18, 19, None)),
                            ),
                        ],
                        TokEof([], '', '', Span(19, 19, None)),
                    ),
                    '''
                    <test_block>:4:2: error: invalid dedent level
                      d
                     ^
                    ''')

    LexerSuite.__name__ += version.replace('.', '')
    LexerSuite.__qualname__ = LexerSuite.__name__
    globals()[LexerSuite.__name__] = LexerSuite

#make_suite('2.0')
#make_suite('2.1')
#make_suite('2.2')
#make_suite('2.3')
#make_suite('2.4')
#make_suite('2.5')
#make_suite('2.6')
make_suite('2.7')
make_suite(sys.version[:3])
#make_suite('3.0')
#make_suite('3.1')
#make_suite('3.2')
#make_suite('3.3')
#make_suite('3.4')
#make_suite('3.5')
#make_suite('3.6')
#make_suite('3.7')
