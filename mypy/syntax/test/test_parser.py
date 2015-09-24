from typing import (
        IO,
        Union,
        cast,
)

from mypy.myunit import Suite, assert_equal, assert_raises
from mypy.syntax.dialect import Dialect
from mypy.syntax.span import CodeMap, FileErrorStream
from mypy.syntax.tokens import Token
from mypy.syntax.tokens import LitBytes, LitUnicode, LitInteger, LitFloat, LitImaginary, TokName
from mypy.syntax.tree import Tree
from mypy.syntax.parser import infer_encoding, Parser

import inspect
import io
import sys
import textwrap


def dump_node(node: Union[Token, Tree, None], stream: IO[str], mirror: IO[str], depth: int = 0) -> None:
    indent = ' ' * depth
    depth += 1
    if node is None:
        stream.writelines([indent, 'None', '\n'])
        return
    if isinstance(node, Token):
        if isinstance(node, (LitBytes, LitUnicode, LitInteger, LitFloat, LitImaginary, TokName)):
            stream.writelines([indent, type(node).__name__, ' ', node._text, '\n'])
        else:
            stream.writelines([indent, type(node).__name__, '\n'])
        for comment in node._comments:
            mirror.writelines([comment._whitespace, comment._text])
        mirror.writelines([node._whitespace, node._text])
        return
    assert isinstance(node, Tree)
    tree = cast(Tree, node)
    stream.writelines([indent, type(node).__name__, '\n'])
    for c in tree._children:
        dump_node(c, stream, mirror, depth)

def make_suite(version: str) -> type:
    dialect = Dialect(version)
    dialect_variations = {
            'KwFalse': 'KwFalse' if dialect.literal_bool else 'TokName False',
            'KwTrue': 'KwTrue' if dialect.literal_bool else 'TokName True',
            'LitStr': 'LitUnicode' if dialect.default_unicode_literals else 'LitBytes',
    }

    def assert_parse(input_txt: str, expected_dump: str, expected_errors: str = '', *, depth: int = 0) -> None:
        input_txt = textwrap.dedent(input_txt.lstrip('\n'))
        expected_dump = textwrap.dedent(expected_dump.lstrip('\n')).format(**dialect_variations)
        expected_errors = textwrap.dedent(expected_errors.lstrip('\n'))

        _frame, _src_filename, _src_line, caller, _src_txt, _src_idx = inspect.stack()[depth + 1]
        errors_stream = io.StringIO()
        cm = CodeMap(FileErrorStream(errors_stream))
        if not input_txt.endswith('\n'):
            input_txt += '\n'

        parser = Parser(cm, dialect)
        output_stream = io.StringIO()
        dump_stream = io.StringIO()

        tree, _file_info, _encoding, _dialect = parser.parse('<%s>' % caller, input_txt)
        dump_node(tree, dump_stream, output_stream)

        assert_equal(expected_dump, dump_stream.getvalue())
        assert_equal(input_txt, output_stream.getvalue())
        assert_equal(expected_errors, errors_stream.getvalue())

    def assert_parse_if(input_txt: str, cond: bool, tree_true: str, tree_false: str, tree_false_errors: str = '', *, depth: int = 0) -> None:
        if cond:
            assert_parse(input_txt, tree_true, depth=depth+1)
        else:
            assert_parse(input_txt, tree_false, tree_false_errors, depth=depth+1)

    class ParserSuite(Suite):
        def test_encoding(self):
            assert_equal(infer_encoding(b'', 'foo'), 'foo')
            assert_equal(infer_encoding(b'coding: bar', 'foo'), 'foo')
            assert_equal(infer_encoding(b'#coding:\nbar', 'foo'), 'foo')
            assert_equal(infer_encoding(b'#coding:bar', 'foo'), 'bar')
            assert_equal(infer_encoding(b'# coding=\t\fbar', 'foo'), 'bar')
            assert_equal(infer_encoding(b'\n#coding: bar', 'foo'), 'bar')
            assert_equal(infer_encoding(b'\n\n#coding: bar', 'foo'), 'foo')
            assert_raises(ValueError, 'multiple `coding:` comments',
                    infer_encoding, (b'#coding: coding: bar', 'foo'))
            assert_raises(ValueError, 'multiple `coding:` comments',
                    infer_encoding, (b'#coding: bar coding: bar', 'foo'))
            assert_raises(ValueError, 'multiple `coding:` comments',
                    infer_encoding, (b'#coding: bar coding: baz', 'foo'))
            assert_raises(ValueError, 'multiple `coding:` comments',
                    infer_encoding, (b'#coding: bar\n#coding: bar', 'foo'))
            assert_raises(ValueError, 'multiple `coding:` comments',
                    infer_encoding, (b'#coding: bar\n#coding: baz', 'foo'))
            assert_equal(infer_encoding(b'#coding: utf-8', 'foo'), 'utf-8')
            assert_equal(infer_encoding(b'\xef\xbb\xbf', 'foo'), 'utf-8-sig')
            assert_equal(infer_encoding(b'\xef\xbb\xbf#coding: utf-8', 'foo'), 'utf-8-sig')
            assert_raises(ValueError, '`coding: ascii` conflicts with BOM',
                    infer_encoding, (b'\xef\xbb\xbf#coding: ascii', 'foo'))

        def test_empty(self):
            assert_parse('',
                    '''
                    FileInput
                     None
                     TokEof
                    ''')

        def test_pass(self):
            assert_parse('pass',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         PassStmt
                          KwPass
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_none(self):
            assert_parse(
                    'None',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          KwNone
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_error_suite(self):
            assert_parse(
                    '''
                    if True:
                        pass
                    finally:
                        pass
                    ''',
                    '''
                    FileInput
                     TreeList
                      IfStmt
                       IfSuite
                        KwIf
                        {KwTrue}
                        OpColon
                        ComplexSuite
                         TokNewline
                         TokIndent
                         TreeList
                          SimpleStmt
                           TreeList
                            SmallStmtAndOpSemicolon
                             PassStmt
                              KwPass
                             None
                           TokNewline
                         TokDedent
                       None
                       None
                      TreeRecovered
                       TreeError
                        FinallySuite
                         KwFinally
                         OpColon
                         ComplexSuite
                          TokNewline
                          TokIndent
                          TreeList
                           SimpleStmt
                            TreeList
                             SmallStmtAndOpSemicolon
                              PassStmt
                               KwPass
                              None
                            TokNewline
                          TokDedent
                     TokEof
                    ''',
                    '''
                    <test_error_suite>:3:1: error: Unexpected KwFinally, expected one of {<any simple statement>, KwAsync, KwClass, KwDef, KwElif, KwElse, KwFor, KwIf, KwTry, KwWhile, KwWith, OpAt, TokEof}.
                    finally:
                    ^~~~~~~
                    '''
                    )

        def test_expr_name(self):
            assert_parse('foo',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          TokName foo
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_expr_lit_str(self):
            assert_parse('"abc"',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          LitStrings
                           TreeList
                            {LitStr} "abc"
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('"abc" r"def"',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          LitStrings
                           TreeList
                            {LitStr} "abc"
                            {LitStr} r"def"
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('b"abc" br"def"',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          LitStrings
                           TreeList
                            LitBytes b"abc"
                            LitBytes br"def"
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse_if('b"abc" u"def"', dialect.mixed_strings,
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          LitStrings
                           TreeList
                            LitBytes b"abc"
                            LitUnicode u"def"
                         None
                       TokNewline
                     TokEof
                    ''',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          TreeRecovered
                           TreeError
                            LitStrings
                             TreeList
                              LitBytes b"abc"
                              LitUnicode u"def"
                         None
                       TokNewline
                     TokEof
                    ''',
                    '''
                    <test_expr_lit_str>:1:8: error: mixed bytes and unicode strings not allowed
                    b"abc" u"def"
                           ^~~~~~
                    ''')
            assert_parse_if('u"abc" u"def"', dialect.unicode_literals,
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          LitStrings
                           TreeList
                            LitUnicode u"abc"
                            LitUnicode u"def"
                         None
                       TokNewline
                     TokEof
                    ''',
                    '''
                    false tree nyi
                    ''',
                    '''
                    errors nyi
                    ''')

        def test_expr_lit_num(self):
            assert_parse('123',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          LitInteger 123
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('123.456',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          LitFloat 123.456
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('123.456j',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          LitImaginary 123.456j
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_expr_paren(self):
            assert_parse('(yield)',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          YieldAtom
                           OpLPar
                           SimpleYieldExpr
                            KwYield
                            None
                           OpRPar
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('(yield None)',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          YieldAtom
                           OpLPar
                           SimpleYieldExpr
                            KwYield
                            KwNone
                           OpRPar
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('(yield from None)',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          YieldAtom
                           OpLPar
                           YieldFromExpr
                            KwYield
                            KwFrom
                            KwNone
                           OpRPar
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('()',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          ParenthForm
                           OpLPar
                           None
                           OpRPar
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('(None)',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          ParenthForm
                           OpLPar
                           KwNone
                           OpRPar
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('(None,)',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          ParenthForm
                           OpLPar
                           CommaSequenceItem
                            TreeList
                             SequenceItemAndOpComma
                              KwNone
                              OpComma
                           OpRPar
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('(None, None)',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          ParenthForm
                           OpLPar
                           CommaSequenceItem
                            TreeList
                             SequenceItemAndOpComma
                              KwNone
                              OpComma
                             SequenceItemAndOpComma
                              KwNone
                              None
                           OpRPar
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('(None, None,)',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          ParenthForm
                           OpLPar
                           CommaSequenceItem
                            TreeList
                             SequenceItemAndOpComma
                              KwNone
                              OpComma
                             SequenceItemAndOpComma
                              KwNone
                              OpComma
                           OpRPar
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('(None for x in y for a in b if z)',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          GeneratorExpr
                           OpLPar
                           KwNone
                           CompFor
                            KwFor
                            TokName x
                            KwIn
                            TokName y
                            CompFor
                             KwFor
                             TokName a
                             KwIn
                             TokName b
                             CompIf
                              KwIf
                              TokName z
                              None
                           OpRPar
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_expr_sqb(self):
            assert_parse('[]',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          ListLiteral
                           OpLSqb
                           None
                           OpRSqb
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('[None]',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          ListLiteral
                           OpLSqb
                           TreeList
                            SequenceItemAndOpComma
                             KwNone
                             None
                           OpRSqb
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('[None,]',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          ListLiteral
                           OpLSqb
                           TreeList
                            SequenceItemAndOpComma
                             KwNone
                             OpComma
                           OpRSqb
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('[None, None]',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          ListLiteral
                           OpLSqb
                           TreeList
                            SequenceItemAndOpComma
                             KwNone
                             OpComma
                            SequenceItemAndOpComma
                             KwNone
                             None
                           OpRSqb
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('[None, None,]',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          ListLiteral
                           OpLSqb
                           TreeList
                            SequenceItemAndOpComma
                             KwNone
                             OpComma
                            SequenceItemAndOpComma
                             KwNone
                             OpComma
                           OpRSqb
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('[None for x in y if z for a in b]',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          ListComprehension
                           OpLSqb
                           KwNone
                           ListFor
                            KwFor
                            TokName x
                            KwIn
                            TokName y
                            ListIf
                             KwIf
                             TokName z
                             ListFor
                              KwFor
                              TokName a
                              KwIn
                              TokName b
                              None
                           OpRSqb
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('[*0]',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          ListLiteral
                           OpLSqb
                           TreeList
                            SequenceItemAndOpComma
                             StarTest
                              OpStar
                              LitInteger 0
                             None
                           OpRSqb
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('[*0, None]',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          ListLiteral
                           OpLSqb
                           TreeList
                            SequenceItemAndOpComma
                             StarTest
                              OpStar
                              LitInteger 0
                             OpComma
                            SequenceItemAndOpComma
                             KwNone
                             None
                           OpRSqb
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('[None, *0]',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          ListLiteral
                           OpLSqb
                           TreeList
                            SequenceItemAndOpComma
                             KwNone
                             OpComma
                            SequenceItemAndOpComma
                             StarTest
                              OpStar
                              LitInteger 0
                             None
                           OpRSqb
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_expr_brace_set(self):
            assert_parse('{None}',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          SetLiteral
                           OpLBrace
                           TreeList
                            SequenceItemAndOpComma
                             KwNone
                             None
                           OpRBrace
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('{None,}',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          SetLiteral
                           OpLBrace
                           TreeList
                            SequenceItemAndOpComma
                             KwNone
                             OpComma
                           OpRBrace
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('{None, None}',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          SetLiteral
                           OpLBrace
                           TreeList
                            SequenceItemAndOpComma
                             KwNone
                             OpComma
                            SequenceItemAndOpComma
                             KwNone
                             None
                           OpRBrace
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('{None, None,}',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          SetLiteral
                           OpLBrace
                           TreeList
                            SequenceItemAndOpComma
                             KwNone
                             OpComma
                            SequenceItemAndOpComma
                             KwNone
                             OpComma
                           OpRBrace
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('{None for x in y if z for a in b}',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          SetComprehension
                           OpLBrace
                           KwNone
                           CompFor
                            KwFor
                            TokName x
                            KwIn
                            TokName y
                            CompIf
                             KwIf
                             TokName z
                             CompFor
                              KwFor
                              TokName a
                              KwIn
                              TokName b
                              None
                           OpRBrace
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('{*0}',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          SetLiteral
                           OpLBrace
                           TreeList
                            SequenceItemAndOpComma
                             StarTest
                              OpStar
                              LitInteger 0
                             None
                           OpRBrace
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('{*0, None}',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          SetLiteral
                           OpLBrace
                           TreeList
                            SequenceItemAndOpComma
                             StarTest
                              OpStar
                              LitInteger 0
                             OpComma
                            SequenceItemAndOpComma
                             KwNone
                             None
                           OpRBrace
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('{None, *0}',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          SetLiteral
                           OpLBrace
                           TreeList
                            SequenceItemAndOpComma
                             KwNone
                             OpComma
                            SequenceItemAndOpComma
                             StarTest
                              OpStar
                              LitInteger 0
                             None
                           OpRBrace
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_expr_brace_dict(self):
            assert_parse('{}',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          DictLiteral
                           OpLBrace
                           None
                           OpRBrace
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('{0: None}',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          DictLiteral
                           OpLBrace
                           TreeList
                            MappingItemAndOpComma
                             TestColonTest
                              LitInteger 0
                              OpColon
                              KwNone
                             None
                           OpRBrace
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('{0: None,}',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          DictLiteral
                           OpLBrace
                           TreeList
                            MappingItemAndOpComma
                             TestColonTest
                              LitInteger 0
                              OpColon
                              KwNone
                             OpComma
                           OpRBrace
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('{0: None, 0: None}',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          DictLiteral
                           OpLBrace
                           TreeList
                            MappingItemAndOpComma
                             TestColonTest
                              LitInteger 0
                              OpColon
                              KwNone
                             OpComma
                            MappingItemAndOpComma
                             TestColonTest
                              LitInteger 0
                              OpColon
                              KwNone
                             None
                           OpRBrace
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('{0: None, 0: None,}',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          DictLiteral
                           OpLBrace
                           TreeList
                            MappingItemAndOpComma
                             TestColonTest
                              LitInteger 0
                              OpColon
                              KwNone
                             OpComma
                            MappingItemAndOpComma
                             TestColonTest
                              LitInteger 0
                              OpColon
                              KwNone
                             OpComma
                           OpRBrace
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('{0: None for x in y if z for a in b}',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          DictComprehension
                           OpLBrace
                           TestColonTest
                            LitInteger 0
                            OpColon
                            KwNone
                           CompFor
                            KwFor
                            TokName x
                            KwIn
                            TokName y
                            CompIf
                             KwIf
                             TokName z
                             CompFor
                              KwFor
                              TokName a
                              KwIn
                              TokName b
                              None
                           OpRBrace
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('{**0}',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          DictLiteral
                           OpLBrace
                           TreeList
                            MappingItemAndOpComma
                             StarStarTest
                              OpStarStar
                              LitInteger 0
                             None
                           OpRBrace
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('{**0, 1: None}',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          DictLiteral
                           OpLBrace
                           TreeList
                            MappingItemAndOpComma
                             StarStarTest
                              OpStarStar
                              LitInteger 0
                             OpComma
                            MappingItemAndOpComma
                             TestColonTest
                              LitInteger 1
                              OpColon
                              KwNone
                             None
                           OpRBrace
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('{1: None, **0}',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          DictLiteral
                           OpLBrace
                           TreeList
                            MappingItemAndOpComma
                             TestColonTest
                              LitInteger 1
                              OpColon
                              KwNone
                             OpComma
                            MappingItemAndOpComma
                             StarStarTest
                              OpStarStar
                              LitInteger 0
                             None
                           OpRBrace
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_expr_attribute(self):
            assert_parse('foo.bar',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          AttributeRef
                           TokName foo
                           OpDot
                           TokName bar
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('foo.bar()[0].baz',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          AttributeRef
                           Subscription
                            CallExpr
                             AttributeRef
                              TokName foo
                              OpDot
                              TokName bar
                             OpLPar
                             None
                             OpRPar
                            OpLSqb
                            LitInteger 0
                            OpRSqb
                           OpDot
                           TokName baz
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_expr_subscript(self):
            assert_parse('foo[0]',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          Subscription
                           TokName foo
                           OpLSqb
                           LitInteger 0
                           OpRSqb
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('foo[0,]',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          Subscription
                           TokName foo
                           OpLSqb
                           CommaSubscript
                            TreeList
                             SubscriptAndOpComma
                              LitInteger 0
                              OpComma
                           OpRSqb
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('foo[:,0]',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          Subscription
                           TokName foo
                           OpLSqb
                           CommaSubscript
                            TreeList
                             SubscriptAndOpComma
                              ProperSlice
                               None
                               OpColon
                               None
                               None
                               None
                              OpComma
                             SubscriptAndOpComma
                              LitInteger 0
                              None
                           OpRSqb
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('foo[:]',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          Subscription
                           TokName foo
                           OpLSqb
                           ProperSlice
                            None
                            OpColon
                            None
                            None
                            None
                           OpRSqb
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('foo[0:0]',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          Subscription
                           TokName foo
                           OpLSqb
                           ProperSlice
                            LitInteger 0
                            OpColon
                            LitInteger 0
                            None
                            None
                           OpRSqb
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('foo[::]',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          Subscription
                           TokName foo
                           OpLSqb
                           ProperSlice
                            None
                            OpColon
                            None
                            OpColon
                            None
                           OpRSqb
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('foo[0:0:0]',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          Subscription
                           TokName foo
                           OpLSqb
                           ProperSlice
                            LitInteger 0
                            OpColon
                            LitInteger 0
                            OpColon
                            LitInteger 0
                           OpRSqb
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_expr_call(self):
            assert_parse('foo()',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          CallExpr
                           TokName foo
                           OpLPar
                           None
                           OpRPar
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('foo(k=None)',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          CallExpr
                           TokName foo
                           OpLPar
                           ArgList
                            TreeList
                             ArgumentAndOpComma
                              KeywordArgument
                               TokName k
                               OpEqual
                               KwNone
                              None
                           OpRPar
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('foo(k=None,)',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          CallExpr
                           TokName foo
                           OpLPar
                           ArgList
                            TreeList
                             ArgumentAndOpComma
                              KeywordArgument
                               TokName k
                               OpEqual
                               KwNone
                              OpComma
                           OpRPar
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('foo(0)',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          CallExpr
                           TokName foo
                           OpLPar
                           ArgList
                            TreeList
                             ArgumentAndOpComma
                              PositionalArgument
                               LitInteger 0
                               None
                              None
                           OpRPar
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('foo(0,)',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          CallExpr
                           TokName foo
                           OpLPar
                           ArgList
                            TreeList
                             ArgumentAndOpComma
                              PositionalArgument
                               LitInteger 0
                               None
                              OpComma
                           OpRPar
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('foo(0, 0)',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          CallExpr
                           TokName foo
                           OpLPar
                           ArgList
                            TreeList
                             ArgumentAndOpComma
                              PositionalArgument
                               LitInteger 0
                               None
                              OpComma
                             ArgumentAndOpComma
                              PositionalArgument
                               LitInteger 0
                               None
                              None
                           OpRPar
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('foo(0, 0,)',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          CallExpr
                           TokName foo
                           OpLPar
                           ArgList
                            TreeList
                             ArgumentAndOpComma
                              PositionalArgument
                               LitInteger 0
                               None
                              OpComma
                             ArgumentAndOpComma
                              PositionalArgument
                               LitInteger 0
                               None
                              OpComma
                           OpRPar
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('foo(0, 1, k=None, v=None,)',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          CallExpr
                           TokName foo
                           OpLPar
                           ArgList
                            TreeList
                             ArgumentAndOpComma
                              PositionalArgument
                               LitInteger 0
                               None
                              OpComma
                             ArgumentAndOpComma
                              PositionalArgument
                               LitInteger 1
                               None
                              OpComma
                             ArgumentAndOpComma
                              KeywordArgument
                               TokName k
                               OpEqual
                               KwNone
                              OpComma
                             ArgumentAndOpComma
                              KeywordArgument
                               TokName v
                               OpEqual
                               KwNone
                              OpComma
                           OpRPar
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('foo(**b)',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          CallExpr
                           TokName foo
                           OpLPar
                           ArgList
                            TreeList
                             ArgumentAndOpComma
                              StarStarTest
                               OpStarStar
                               TokName b
                              None
                           OpRPar
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('foo(*a, **b)',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          CallExpr
                           TokName foo
                           OpLPar
                           ArgList
                            TreeList
                             ArgumentAndOpComma
                              StarTest
                               OpStar
                               TokName a
                              OpComma
                             ArgumentAndOpComma
                              StarStarTest
                               OpStarStar
                               TokName b
                              None
                           OpRPar
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('foo(0, *a, k=None, **b)',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          CallExpr
                           TokName foo
                           OpLPar
                           ArgList
                            TreeList
                             ArgumentAndOpComma
                              PositionalArgument
                               LitInteger 0
                               None
                              OpComma
                             ArgumentAndOpComma
                              StarTest
                               OpStar
                               TokName a
                              OpComma
                             ArgumentAndOpComma
                              KeywordArgument
                               TokName k
                               OpEqual
                               KwNone
                              OpComma
                             ArgumentAndOpComma
                              StarStarTest
                               OpStarStar
                               TokName b
                              None
                           OpRPar
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_expr_await(self):
            assert_parse_if('async def foo(): await None', dialect.coroutines,
                    '''
                    FileInput
                     TreeList
                      AsyncFuncDef
                       KwAsync
                       FuncDef
                        KwDef
                        TokName foo
                        Parameters
                         OpLPar
                         None
                         OpRPar
                        None
                        None
                        OpColon
                        SimpleStmt
                         TreeList
                          SmallStmtAndOpSemicolon
                           ExprStmt
                            AwaitExpr
                             KwAwait
                             KwNone
                           None
                         TokNewline
                     TokEof
                    ''',
                    '''
                    FileInput
                     TreeList
                      TreeRecovered
                       TreeError
                        TreeErrorList
                         TreeError
                          ExprStmt
                           TokName async
                          TreeError
                           KwDef
                           TokName foo
                           TreeError
                            OpLPar
                            OpRPar
                           OpColon
                           TokName await
                           KwNone
                        TokNewline
                     TokEof
                    ''',
                    '''
                    <test_expr_await>:1:7: error: Unexpected KwDef, expected one of {KwAnd, KwIf, KwIn, KwIs, KwNot, KwOr, OpAmper, OpCircumflex, OpClassicSlash, OpComma, OpDot, OpEqEqual, OpGreater, OpGreaterEqual, OpLPar, OpLShift, OpLSqb, OpLess, OpLessEqual, OpMinus, OpNotEqual, OpPercent, OpPlus, OpRShift, OpSemicolon, OpSlash, OpSlashSlash, OpStar, OpStarStar, OpVBar, TokNewline}.
                    async def foo(): await None
                          ^~~
                    ''')

        def test_expr_power(self):
            assert_parse('-1 ** -2',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          MinusFactor
                           OpMinus
                           Power
                            LitInteger 1
                            OpStarStar
                            MinusFactor
                             OpMinus
                             LitInteger 2
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('1 ** 2 ** 3',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          Power
                           LitInteger 1
                           OpStarStar
                           Power
                            LitInteger 2
                            OpStarStar
                            LitInteger 3
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_expr_factor(self):
            assert_parse('+0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          PlusFactor
                           OpPlus
                           LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('-0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          MinusFactor
                           OpMinus
                           LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('~0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          TildeFactor
                           OpTilde
                           LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('+-~0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          PlusFactor
                           OpPlus
                           MinusFactor
                            OpMinus
                            TildeFactor
                             OpTilde
                             LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_expr_mul_expr(self):
            assert_parse('+0 * +0 * +0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          StarTerm
                           StarTerm
                            PlusFactor
                             OpPlus
                             LitInteger 0
                            OpStar
                            PlusFactor
                             OpPlus
                             LitInteger 0
                           OpStar
                           PlusFactor
                            OpPlus
                            LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse_if('0 * 0 / 0 // 0 / 0 * 0', dialect.default_true_division,
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          StarTerm
                           SlashTerm
                            SlashSlashTerm
                             SlashTerm
                              StarTerm
                               LitInteger 0
                               OpStar
                               LitInteger 0
                              OpSlash
                              LitInteger 0
                             OpSlashSlash
                             LitInteger 0
                            OpSlash
                            LitInteger 0
                           OpStar
                           LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          StarTerm
                           ClassicSlashTerm
                            SlashSlashTerm
                             ClassicSlashTerm
                              StarTerm
                               LitInteger 0
                               OpStar
                               LitInteger 0
                              OpClassicSlash
                              LitInteger 0
                             OpSlashSlash
                             LitInteger 0
                            OpClassicSlash
                            LitInteger 0
                           OpStar
                           LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''',
                    '''
                    <test_expr_mul_expr>:1:7: warning: classic division; use true division or floor division instead
                    0 * 0 / 0 // 0 / 0 * 0
                          ^
                    <test_expr_mul_expr>:1:16: warning: classic division; use true division or floor division instead
                    0 * 0 / 0 // 0 / 0 * 0
                                   ^
                    ''')
            assert_parse_if('0 * 0 @ 0 * 0', dialect.matmul,
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          StarTerm
                           AtTerm
                            StarTerm
                             LitInteger 0
                             OpStar
                             LitInteger 0
                            OpAt
                            LitInteger 0
                           OpStar
                           LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''',
                    '''
                    FileInput
                     TreeList
                      TreeRecovered
                       TreeError
                        TreeErrorList
                         TreeError
                          ExprStmt
                           StarTerm
                            LitInteger 0
                            OpStar
                            LitInteger 0
                          TreeError
                           OpAt
                           LitInteger 0
                           OpStar
                           LitInteger 0
                        TokNewline
                     TokEof
                    ''',
                    '''
                    <test_expr_mul_expr>:1:7: error: Unexpected OpAt, expected one of {KwAnd, KwIf, KwIn, KwIs, KwNot, KwOr, OpAmper, OpCircumflex, OpClassicSlash, OpComma, OpDot, OpEqEqual, OpGreater, OpGreaterEqual, OpLPar, OpLShift, OpLSqb, OpLess, OpLessEqual, OpMinus, OpNotEqual, OpPercent, OpPlus, OpRShift, OpSemicolon, OpSlash, OpSlashSlash, OpStar, OpStarStar, OpVBar, TokNewline}.
                    0 * 0 @ 0 * 0
                          ^
                    ''')

        def test_expr_add_expr(self):
            assert_parse('0 * 0 + 0 * 0 + 0 * 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          PlusExpr
                           PlusExpr
                            StarTerm
                             LitInteger 0
                             OpStar
                             LitInteger 0
                            OpPlus
                            StarTerm
                             LitInteger 0
                             OpStar
                             LitInteger 0
                           OpPlus
                           StarTerm
                            LitInteger 0
                            OpStar
                            LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('0 + 0 - 0 + 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          PlusExpr
                           MinusExpr
                            PlusExpr
                             LitInteger 0
                             OpPlus
                             LitInteger 0
                            OpMinus
                            LitInteger 0
                           OpPlus
                           LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_expr_shift_expr(self):
            assert_parse('0 + 0 << 0 + 0 << 0 + 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          LShiftExpr
                           LShiftExpr
                            PlusExpr
                             LitInteger 0
                             OpPlus
                             LitInteger 0
                            OpLShift
                            PlusExpr
                             LitInteger 0
                             OpPlus
                             LitInteger 0
                           OpLShift
                           PlusExpr
                            LitInteger 0
                            OpPlus
                            LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('0 << 0 >> 0 << 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          LShiftExpr
                           RShiftExpr
                            LShiftExpr
                             LitInteger 0
                             OpLShift
                             LitInteger 0
                            OpRShift
                            LitInteger 0
                           OpLShift
                           LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_expr_bitand(self):
            assert_parse('0 << 0 & 0 << 0 & 0 << 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          AndExpr
                           AndExpr
                            LShiftExpr
                             LitInteger 0
                             OpLShift
                             LitInteger 0
                            OpAmper
                            LShiftExpr
                             LitInteger 0
                             OpLShift
                             LitInteger 0
                           OpAmper
                           LShiftExpr
                            LitInteger 0
                            OpLShift
                            LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_expr_bitxor(self):
            assert_parse('0 & 0 ^ 0 & 0 ^ 0 & 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          XorExpr
                           XorExpr
                            AndExpr
                             LitInteger 0
                             OpAmper
                             LitInteger 0
                            OpCircumflex
                            AndExpr
                             LitInteger 0
                             OpAmper
                             LitInteger 0
                           OpCircumflex
                           AndExpr
                            LitInteger 0
                            OpAmper
                            LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_expr_bitor(self):
            assert_parse('0 ^ 0 | 0 ^ 0 | 0 ^ 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          OrExpr
                           OrExpr
                            XorExpr
                             LitInteger 0
                             OpCircumflex
                             LitInteger 0
                            OpVBar
                            XorExpr
                             LitInteger 0
                             OpCircumflex
                             LitInteger 0
                           OpVBar
                           XorExpr
                            LitInteger 0
                            OpCircumflex
                            LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_expr_compare(self):
            assert_parse('0 | 0 == 0 | 0 == 0 | 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          ComparisonExprChain
                           OrExpr
                            LitInteger 0
                            OpVBar
                            LitInteger 0
                           TreeList
                            CompOpAndOrExpr
                             OpEqEqual
                             OrExpr
                              LitInteger 0
                              OpVBar
                              LitInteger 0
                            CompOpAndOrExpr
                             OpEqEqual
                             OrExpr
                              LitInteger 0
                              OpVBar
                              LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('0 < 0 > 0 <= 0 >= 0 == 0 != 0 is 0 is not 0 in 0 not in 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          ComparisonExprChain
                           LitInteger 0
                           TreeList
                            CompOpAndOrExpr
                             OpLess
                             LitInteger 0
                            CompOpAndOrExpr
                             OpGreater
                             LitInteger 0
                            CompOpAndOrExpr
                             OpLessEqual
                             LitInteger 0
                            CompOpAndOrExpr
                             OpGreaterEqual
                             LitInteger 0
                            CompOpAndOrExpr
                             OpEqEqual
                             LitInteger 0
                            CompOpAndOrExpr
                             OpNotEqual
                             LitInteger 0
                            CompOpAndOrExpr
                             KwIs
                             LitInteger 0
                            CompOpAndOrExpr
                             KwsIsNot
                              KwIs
                              KwNot
                             LitInteger 0
                            CompOpAndOrExpr
                             KwIn
                             LitInteger 0
                            CompOpAndOrExpr
                             KwsNotIn
                              KwNot
                              KwIn
                             LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_expr_lognot(self):
            assert_parse('not not 0 == 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          NotTest
                           KwNot
                           NotTest
                            KwNot
                            ComparisonExprChain
                             LitInteger 0
                             TreeList
                              CompOpAndOrExpr
                               OpEqEqual
                               LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_expr_logand(self):
            assert_parse('not 0 and not 0 and not 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          AndTest
                           AndTest
                            NotTest
                             KwNot
                             LitInteger 0
                            KwAnd
                            NotTest
                             KwNot
                             LitInteger 0
                           KwAnd
                           NotTest
                            KwNot
                            LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_expr_logor(self):
            assert_parse('0 and 0 or 0 and 0 or 0 and 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          OrTest
                           OrTest
                            AndTest
                             LitInteger 0
                             KwAnd
                             LitInteger 0
                            KwOr
                            AndTest
                             LitInteger 0
                             KwAnd
                             LitInteger 0
                           KwOr
                           AndTest
                            LitInteger 0
                            KwAnd
                            LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_expr_cond(self):
            assert_parse('0 if 0 else 0 if 0 else 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          TernaryExpr
                           LitInteger 0
                           KwIf
                           LitInteger 0
                           KwElse
                           TernaryExpr
                            LitInteger 0
                            KwIf
                            LitInteger 0
                            KwElse
                            LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_expr_lambda(self):
            assert_parse('lambda: 0 if 0 else 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          LambdaExpr
                           KwLambda
                           None
                           OpColon
                           TernaryExpr
                            LitInteger 0
                            KwIf
                            LitInteger 0
                            KwElse
                            LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('lambda x: 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          LambdaExpr
                           KwLambda
                           VarArgsList
                            TreeList
                             VarArg
                              Vname
                               TokName x
                              None
                              None
                              None
                            None
                            None
                            None
                            None
                            None
                            None
                           OpColon
                           LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('lambda x,: 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          LambdaExpr
                           KwLambda
                           VarArgsList
                            TreeList
                             VarArg
                              Vname
                               TokName x
                              None
                              None
                              OpComma
                            None
                            None
                            None
                            None
                            None
                            None
                           OpColon
                           LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('lambda x, y: 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          LambdaExpr
                           KwLambda
                           VarArgsList
                            TreeList
                             VarArg
                              Vname
                               TokName x
                              None
                              None
                              OpComma
                             VarArg
                              Vname
                               TokName y
                              None
                              None
                              None
                            None
                            None
                            None
                            None
                            None
                            None
                           OpColon
                           LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('lambda x, y,: 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          LambdaExpr
                           KwLambda
                           VarArgsList
                            TreeList
                             VarArg
                              Vname
                               TokName x
                              None
                              None
                              OpComma
                             VarArg
                              Vname
                               TokName y
                              None
                              None
                              OpComma
                            None
                            None
                            None
                            None
                            None
                            None
                           OpColon
                           LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('lambda *z, a: 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          LambdaExpr
                           KwLambda
                           VarArgsList
                            None
                            OpStar
                            Vname
                             TokName z
                            OpComma
                            TreeList
                             VarArg
                              Vname
                               TokName a
                              None
                              None
                              None
                            None
                            None
                           OpColon
                           LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('lambda *z, **c: 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          LambdaExpr
                           KwLambda
                           VarArgsList
                            None
                            OpStar
                            Vname
                             TokName z
                            OpComma
                            None
                            OpStarStar
                            Vname
                             TokName c
                           OpColon
                           LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('lambda **c: 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          LambdaExpr
                           KwLambda
                           VarArgsList
                            None
                            None
                            None
                            None
                            None
                            OpStarStar
                            Vname
                             TokName c
                           OpColon
                           LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('lambda x, y=0, *, a=0, b, **c: 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          LambdaExpr
                           KwLambda
                           VarArgsList
                            TreeList
                             VarArg
                              Vname
                               TokName x
                              None
                              None
                              OpComma
                             VarArg
                              Vname
                               TokName y
                              OpEqual
                              LitInteger 0
                              OpComma
                            OpStar
                            None
                            OpComma
                            TreeList
                             VarArg
                              Vname
                               TokName a
                              OpEqual
                              LitInteger 0
                              OpComma
                             VarArg
                              Vname
                               TokName b
                              None
                              None
                              OpComma
                            OpStarStar
                            Vname
                             TokName c
                           OpColon
                           LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_expr_nocond(self):
            assert_parse('{x for y in z if lambda: lambda: 0 or 0}',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          SetComprehension
                           OpLBrace
                           TokName x
                           CompFor
                            KwFor
                            TokName y
                            KwIn
                            TokName z
                            CompIf
                             KwIf
                             LambdaExprNocond
                              KwLambda
                              None
                              OpColon
                              LambdaExprNocond
                               KwLambda
                               None
                               OpColon
                               OrTest
                                LitInteger 0
                                KwOr
                                LitInteger 0
                             None
                           OpRBrace
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_exprlist(self):
            assert_parse('0 if 0 else lambda: 0,',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          CommaExpr
                           TreeList
                            TestAndOpComma
                             TernaryExpr
                              LitInteger 0
                              KwIf
                              LitInteger 0
                              KwElse
                              LambdaExpr
                               KwLambda
                               None
                               OpColon
                               LitInteger 0
                             OpComma
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('0, 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          CommaExpr
                           TreeList
                            TestAndOpComma
                             LitInteger 0
                             OpComma
                            TestAndOpComma
                             LitInteger 0
                             None
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('0, 0,',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          CommaExpr
                           TreeList
                            TestAndOpComma
                             LitInteger 0
                             OpComma
                            TestAndOpComma
                             LitInteger 0
                             OpComma
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_stmt_assign(self):
            assert_parse('(x) = 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         AssignStmt
                          TreeList
                           TargetListAndOpEqual
                            ParenTargetList
                             OpLPar
                             TokName x
                             OpRPar
                            OpEqual
                          LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('x, y = z, = a = 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         AssignStmt
                          TreeList
                           TargetListAndOpEqual
                            CommaTarget
                             TreeList
                              TargetAndOpComma
                               TokName x
                               OpComma
                              TargetAndOpComma
                               TokName y
                               None
                            OpEqual
                           TargetListAndOpEqual
                            CommaTarget
                             TreeList
                              TargetAndOpComma
                               TokName z
                               OpComma
                            OpEqual
                           TargetListAndOpEqual
                            TokName a
                            OpEqual
                          LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('*a = *b, c = d, *e = *(f, *g) = 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         AssignStmt
                          TreeList
                           TargetListAndOpEqual
                            StarTarget
                             OpStar
                             TokName a
                            OpEqual
                           TargetListAndOpEqual
                            CommaTarget
                             TreeList
                              TargetAndOpComma
                               StarTarget
                                OpStar
                                TokName b
                               OpComma
                              TargetAndOpComma
                               TokName c
                               None
                            OpEqual
                           TargetListAndOpEqual
                            CommaTarget
                             TreeList
                              TargetAndOpComma
                               TokName d
                               OpComma
                              TargetAndOpComma
                               StarTarget
                                OpStar
                                TokName e
                               None
                            OpEqual
                           TargetListAndOpEqual
                            StarTarget
                             OpStar
                             ParenTargetList
                              OpLPar
                              CommaTarget
                               TreeList
                                TargetAndOpComma
                                 TokName f
                                 OpComma
                                TargetAndOpComma
                                 StarTarget
                                  OpStar
                                  TokName g
                                 None
                              OpRPar
                            OpEqual
                          LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('a().b = (c, d)[e] = 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         AssignStmt
                          TreeList
                           TargetListAndOpEqual
                            AttributeRef
                             CallExpr
                              TokName a
                              OpLPar
                              None
                              OpRPar
                             OpDot
                             TokName b
                            OpEqual
                           TargetListAndOpEqual
                            Subscription
                             ParenthForm
                              OpLPar
                              CommaSequenceItem
                               TreeList
                                SequenceItemAndOpComma
                                 TokName c
                                 OpComma
                                SequenceItemAndOpComma
                                 TokName d
                                 None
                              OpRPar
                             OpLSqb
                             TokName e
                             OpRSqb
                            OpEqual
                          LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_stmt_augassign(self):
            assert_parse('(x) += 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         AugmentedAssignStmt
                          ParenAugTarget
                           OpLPar
                           TokName x
                           OpRPar
                          OpPlusEqual
                          LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('(x) -= 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         AugmentedAssignStmt
                          ParenAugTarget
                           OpLPar
                           TokName x
                           OpRPar
                          OpMinusEqual
                          LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('(x) *= 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         AugmentedAssignStmt
                          ParenAugTarget
                           OpLPar
                           TokName x
                           OpRPar
                          OpStarEqual
                          LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse_if('(x) @= 0', dialect.matmul,
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         AugmentedAssignStmt
                          ParenAugTarget
                           OpLPar
                           TokName x
                           OpRPar
                          OpAtEqual
                          LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''', '''
                    FileInput
                     TreeList
                      TreeRecovered
                       TreeError
                        TreeErrorList
                         TreeError
                          TreeError
                           ParenthForm
                            OpLPar
                            TokName x
                            OpRPar
                          TreeError
                           OpAt
                           OpEqual
                          LitInteger 0
                        TokNewline
                     TokEof
                    ''', '''
                    <test_stmt_augassign>:1:1: error: expression not allowed as lvalue
                    (x) @= 0
                    ^~~
                    <test_stmt_augassign>:1:5: error: Unexpected OpAt, expected one of {OpAmper, OpCircumflex, OpClassicSlash, OpComma, OpDot, OpEqual, OpLPar, OpLShift, OpLSqb, OpMinus, OpPercent, OpPlus, OpRShift, OpSlash, OpSlashSlash, OpStar, OpStarStar, OpVBar}.
                    (x) @= 0
                        ^
                    ''')
            assert_parse_if('x @= 0', dialect.matmul,
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         AugmentedAssignStmt
                          TokName x
                          OpAtEqual
                          LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''', '''
                    FileInput
                     TreeList
                      TreeRecovered
                       TreeError
                        TreeErrorList
                         TreeError
                          TokName x
                          TreeError
                           OpAt
                           OpEqual
                          LitInteger 0
                        TokNewline
                     TokEof
                    ''', '''
                    <test_stmt_augassign>:1:3: error: Unexpected OpAt, expected one of {OpAmper, OpCircumflex, OpClassicSlash, OpComma, OpDot, OpEqual, OpLPar, OpLShift, OpLSqb, OpMinus, OpPercent, OpPlus, OpRShift, OpSlash, OpSlashSlash, OpStar, OpStarStar, OpVBar}.
                    x @= 0
                      ^
                    ''')
            assert_parse_if('(x) /= 0', dialect.default_true_division,
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         AugmentedAssignStmt
                          ParenAugTarget
                           OpLPar
                           TokName x
                           OpRPar
                          OpSlashEqual
                          LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''', '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         AugmentedAssignStmt
                          ParenAugTarget
                           OpLPar
                           TokName x
                           OpRPar
                          OpClassicSlashEqual
                          LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''', '''
                    <test_stmt_augassign>:1:5: warning: classic division; use true division or floor division instead
                    (x) /= 0
                        ^~
                    ''')
            assert_parse('(x) //= 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         AugmentedAssignStmt
                          ParenAugTarget
                           OpLPar
                           TokName x
                           OpRPar
                          OpSlashSlashEqual
                          LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('(x) %= 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         AugmentedAssignStmt
                          ParenAugTarget
                           OpLPar
                           TokName x
                           OpRPar
                          OpPercentEqual
                          LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('(x) **= 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         AugmentedAssignStmt
                          ParenAugTarget
                           OpLPar
                           TokName x
                           OpRPar
                          OpStarStarEqual
                          LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('(x) >>= 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         AugmentedAssignStmt
                          ParenAugTarget
                           OpLPar
                           TokName x
                           OpRPar
                          OpRShiftEqual
                          LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('(x) <<= 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         AugmentedAssignStmt
                          ParenAugTarget
                           OpLPar
                           TokName x
                           OpRPar
                          OpLShiftEqual
                          LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('(x) &= 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         AugmentedAssignStmt
                          ParenAugTarget
                           OpLPar
                           TokName x
                           OpRPar
                          OpAmperEqual
                          LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('(x) ^= 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         AugmentedAssignStmt
                          ParenAugTarget
                           OpLPar
                           TokName x
                           OpRPar
                          OpCircumflexEqual
                          LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('(x) |= 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         AugmentedAssignStmt
                          ParenAugTarget
                           OpLPar
                           TokName x
                           OpRPar
                          OpVBarEqual
                          LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('a().b += 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         AugmentedAssignStmt
                          AttributeRef
                           CallExpr
                            TokName a
                            OpLPar
                            None
                            OpRPar
                           OpDot
                           TokName b
                          OpPlusEqual
                          LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('(c, d)[e] += 0',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         AugmentedAssignStmt
                          Subscription
                           ParenthForm
                            OpLPar
                            CommaSequenceItem
                             TreeList
                              SequenceItemAndOpComma
                               TokName c
                               OpComma
                              SequenceItemAndOpComma
                               TokName d
                               None
                            OpRPar
                           OpLSqb
                           TokName e
                           OpRSqb
                          OpPlusEqual
                          LitInteger 0
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_stmt_assert(self):
            assert_parse('assert x',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         AssertStmt
                          KwAssert
                          TokName x
                          None
                          None
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('assert x, y',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         AssertStmt
                          KwAssert
                          TokName x
                          OpComma
                          TokName y
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_stmt_del(self):
            assert_parse('del a',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         DelStmt
                          KwDel
                          TokName a
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('del a,',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         DelStmt
                          KwDel
                          CommaTarget
                           TreeList
                            TargetAndOpComma
                             TokName a
                             OpComma
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('del a, b',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         DelStmt
                          KwDel
                          CommaTarget
                           TreeList
                            TargetAndOpComma
                             TokName a
                             OpComma
                            TargetAndOpComma
                             TokName b
                             None
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('del a, b,',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         DelStmt
                          KwDel
                          CommaTarget
                           TreeList
                            TargetAndOpComma
                             TokName a
                             OpComma
                            TargetAndOpComma
                             TokName b
                             OpComma
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('del a.b, c[d]',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         DelStmt
                          KwDel
                          CommaTarget
                           TreeList
                            TargetAndOpComma
                             AttributeRef
                              TokName a
                              OpDot
                              TokName b
                             OpComma
                            TargetAndOpComma
                             Subscription
                              TokName c
                              OpLSqb
                              TokName d
                              OpRSqb
                             None
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_stmt_return(self):
            assert_parse('return',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ReturnStmt
                          KwReturn
                          None
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('return x',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ReturnStmt
                          KwReturn
                          TokName x
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('return x,',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ReturnStmt
                          KwReturn
                          CommaExpr
                           TreeList
                            TestAndOpComma
                             TokName x
                             OpComma
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('return x, y',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ReturnStmt
                          KwReturn
                          CommaExpr
                           TreeList
                            TestAndOpComma
                             TokName x
                             OpComma
                            TestAndOpComma
                             TokName y
                             None
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('return x, y,',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ReturnStmt
                          KwReturn
                          CommaExpr
                           TreeList
                            TestAndOpComma
                             TokName x
                             OpComma
                            TestAndOpComma
                             TokName y
                             OpComma
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_stmt_yield(self):
            assert_parse('yield',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         YieldStmt
                          SimpleYieldExpr
                           KwYield
                           None
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('yield None',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         YieldStmt
                          SimpleYieldExpr
                           KwYield
                           KwNone
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('yield from None',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         YieldStmt
                          YieldFromExpr
                           KwYield
                           KwFrom
                           KwNone
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_stmt_print(self):
            assert_parse_if('print', dialect.print_keyword,
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         PrintStmt
                          KwPrint
                          None
                          None
                          None
                          None
                         None
                       TokNewline
                     TokEof
                    ''',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          TokName print
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse_if('print x', dialect.print_keyword,
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         PrintStmt
                          KwPrint
                          None
                          None
                          None
                          TreeList
                           TestAndOpComma
                            TokName x
                            None
                         None
                       TokNewline
                     TokEof
                    ''',
                    '''
                    FileInput
                     TreeList
                      TreeRecovered
                       TreeError
                        TreeErrorList
                         TreeError
                          ExprStmt
                           TokName print
                          TreeError
                           TokName x
                        TokNewline
                     TokEof
                    ''',
                    '''
                    <test_stmt_print>:1:7: error: Unexpected TokName, expected one of {KwAnd, KwIf, KwIn, KwIs, KwNot, KwOr, OpAmper, OpAt, OpCircumflex, OpClassicSlash, OpComma, OpDot, OpEqEqual, OpGreater, OpGreaterEqual, OpLPar, OpLShift, OpLSqb, OpLess, OpLessEqual, OpMinus, OpNotEqual, OpPercent, OpPlus, OpRShift, OpSemicolon, OpSlash, OpSlashSlash, OpStar, OpStarStar, OpVBar, TokNewline}.
                    print x
                          ^
                    ''')
            assert_parse_if('print x,', dialect.print_keyword,
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         PrintStmt
                          KwPrint
                          None
                          None
                          None
                          TreeList
                           TestAndOpComma
                            TokName x
                            OpComma
                         None
                       TokNewline
                     TokEof
                    ''',
                    '''
                    FileInput
                     TreeList
                      TreeRecovered
                       TreeError
                        TreeErrorList
                         TreeError
                          ExprStmt
                           TokName print
                          TreeError
                           TokName x
                           OpComma
                        TokNewline
                     TokEof
                    ''',
                    '''
                    <test_stmt_print>:1:7: error: Unexpected TokName, expected one of {KwAnd, KwIf, KwIn, KwIs, KwNot, KwOr, OpAmper, OpAt, OpCircumflex, OpClassicSlash, OpComma, OpDot, OpEqEqual, OpGreater, OpGreaterEqual, OpLPar, OpLShift, OpLSqb, OpLess, OpLessEqual, OpMinus, OpNotEqual, OpPercent, OpPlus, OpRShift, OpSemicolon, OpSlash, OpSlashSlash, OpStar, OpStarStar, OpVBar, TokNewline}.
                    print x,
                          ^
                    ''')
            assert_parse_if('print x, y', dialect.print_keyword,
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         PrintStmt
                          KwPrint
                          None
                          None
                          None
                          TreeList
                           TestAndOpComma
                            TokName x
                            OpComma
                           TestAndOpComma
                            TokName y
                            None
                         None
                       TokNewline
                     TokEof
                    ''',
                    '''
                    FileInput
                     TreeList
                      TreeRecovered
                       TreeError
                        TreeErrorList
                         TreeError
                          ExprStmt
                           TokName print
                          TreeError
                           TokName x
                           OpComma
                           TokName y
                        TokNewline
                     TokEof
                    ''',
                    '''
                    <test_stmt_print>:1:7: error: Unexpected TokName, expected one of {KwAnd, KwIf, KwIn, KwIs, KwNot, KwOr, OpAmper, OpAt, OpCircumflex, OpClassicSlash, OpComma, OpDot, OpEqEqual, OpGreater, OpGreaterEqual, OpLPar, OpLShift, OpLSqb, OpLess, OpLessEqual, OpMinus, OpNotEqual, OpPercent, OpPlus, OpRShift, OpSemicolon, OpSlash, OpSlashSlash, OpStar, OpStarStar, OpVBar, TokNewline}.
                    print x, y
                          ^
                    ''')
            assert_parse_if('print x, y,', dialect.print_keyword,
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         PrintStmt
                          KwPrint
                          None
                          None
                          None
                          TreeList
                           TestAndOpComma
                            TokName x
                            OpComma
                           TestAndOpComma
                            TokName y
                            OpComma
                         None
                       TokNewline
                     TokEof
                    ''',
                    '''
                    FileInput
                     TreeList
                      TreeRecovered
                       TreeError
                        TreeErrorList
                         TreeError
                          ExprStmt
                           TokName print
                          TreeError
                           TokName x
                           OpComma
                           TokName y
                           OpComma
                        TokNewline
                     TokEof
                    ''',
                    '''
                    <test_stmt_print>:1:7: error: Unexpected TokName, expected one of {KwAnd, KwIf, KwIn, KwIs, KwNot, KwOr, OpAmper, OpAt, OpCircumflex, OpClassicSlash, OpComma, OpDot, OpEqEqual, OpGreater, OpGreaterEqual, OpLPar, OpLShift, OpLSqb, OpLess, OpLessEqual, OpMinus, OpNotEqual, OpPercent, OpPlus, OpRShift, OpSemicolon, OpSlash, OpSlashSlash, OpStar, OpStarStar, OpVBar, TokNewline}.
                    print x, y,
                          ^
                    ''')
            assert_parse_if('print ()', dialect.print_keyword,
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         PrintStmt
                          KwPrint
                          None
                          None
                          None
                          TreeList
                           TestAndOpComma
                            ParenthForm
                             OpLPar
                             None
                             OpRPar
                            None
                         None
                       TokNewline
                     TokEof
                    ''',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          CallExpr
                           TokName print
                           OpLPar
                           None
                           OpRPar
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse_if('print (x)', dialect.print_keyword,
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         PrintStmt
                          KwPrint
                          None
                          None
                          None
                          TreeList
                           TestAndOpComma
                            ParenthForm
                             OpLPar
                             TokName x
                             OpRPar
                            None
                         None
                       TokNewline
                     TokEof
                    ''',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          CallExpr
                           TokName print
                           OpLPar
                           ArgList
                            TreeList
                             ArgumentAndOpComma
                              PositionalArgument
                               TokName x
                               None
                              None
                           OpRPar
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse_if('print (x,)', dialect.print_keyword,
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         PrintStmt
                          KwPrint
                          None
                          None
                          None
                          TreeList
                           TestAndOpComma
                            ParenthForm
                             OpLPar
                             CommaSequenceItem
                              TreeList
                               SequenceItemAndOpComma
                                TokName x
                                OpComma
                             OpRPar
                            None
                         None
                       TokNewline
                     TokEof
                    ''',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          CallExpr
                           TokName print
                           OpLPar
                           ArgList
                            TreeList
                             ArgumentAndOpComma
                              PositionalArgument
                               TokName x
                               None
                              OpComma
                           OpRPar
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse_if('print (x, y)', dialect.print_keyword,
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         PrintStmt
                          KwPrint
                          None
                          None
                          None
                          TreeList
                           TestAndOpComma
                            ParenthForm
                             OpLPar
                             CommaSequenceItem
                              TreeList
                               SequenceItemAndOpComma
                                TokName x
                                OpComma
                               SequenceItemAndOpComma
                                TokName y
                                None
                             OpRPar
                            None
                         None
                       TokNewline
                     TokEof
                    ''',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         ExprStmt
                          CallExpr
                           TokName print
                           OpLPar
                           ArgList
                            TreeList
                             ArgumentAndOpComma
                              PositionalArgument
                               TokName x
                               None
                              OpComma
                             ArgumentAndOpComma
                              PositionalArgument
                               TokName y
                               None
                              None
                           OpRPar
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_stmt_raise(self):
            assert_parse('raise',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         SimpleRaiseStmt
                          KwRaise
                          None
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('raise x',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         SimpleRaiseStmt
                          KwRaise
                          TokName x
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('raise x from y',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         RaiseFromStmt
                          KwRaise
                          TokName x
                          KwFrom
                          TokName y
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('raise x, y',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         LegacyRaiseStmt
                          KwRaise
                          TokName x
                          OpComma
                          TokName y
                          None
                          None
                         None
                       TokNewline
                     TokEof
                    ''')
            assert_parse('raise x, y, z',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         LegacyRaiseStmt
                          KwRaise
                          TokName x
                          OpComma
                          TokName y
                          OpComma
                          TokName z
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_stmt_(self):
            assert_parse('pass',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         PassStmt
                          KwPass
                         None
                       TokNewline
                     TokEof
                    ''')

        def test_nyi(self):
            assert_parse('pass',
                    '''
                    FileInput
                     TreeList
                      SimpleStmt
                       TreeList
                        SmallStmtAndOpSemicolon
                         PassStmt
                          KwPass
                         None
                       TokNewline
                     TokEof
                    ''')

    ParserSuite.__name__ += version.replace('.', '')
    ParserSuite.__qualname__ = ParserSuite.__name__
    globals()[ParserSuite.__name__] = ParserSuite

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
