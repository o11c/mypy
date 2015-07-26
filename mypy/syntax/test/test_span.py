from io import StringIO

import typing

from mypy.myunit import Suite, assert_equal, assert_raises
from mypy.syntax.span import (
        Bisector,
        CodeMap,
        FileErrorStream,
        ForbidErrorStream,
        HumanLocation,
        Span,
)


def CM() -> CodeMap:
    return CodeMap(ForbidErrorStream())


class BisectorSuite(Suite):

    def test_empty(self):
        b = Bisector() # type: Bisector[str]
        assert_raises(KeyError, -2, b.lookup, [-2])
        assert_raises(KeyError, -1, b.lookup, [-1])
        assert_raises(KeyError, 0, b.lookup, [0])
        assert_raises(KeyError, 1, b.lookup, [1])

    def test_single(self):
        b = Bisector() # type: Bisector[str]
        b.add(3, 'hello')
        assert_raises(KeyError, -2, b.lookup, [-2])
        assert_raises(KeyError, -1, b.lookup, [-1])
        assert_equal(b.lookup(0), ('hello', 0, 0, 3))
        assert_equal(b.lookup(1), ('hello', 1, 0, 3))
        assert_equal(b.lookup(2), ('hello', 2, 0, 3))
        assert_raises(KeyError, 3, b.lookup, [3])
        assert_raises(KeyError, 4, b.lookup, [4])

    def test_double(self):
        b = Bisector() # type: Bisector[str]
        b.add(3, 'hello')
        b.add(4, 'world')
        assert_raises(KeyError, -2, b.lookup, [-2])
        assert_raises(KeyError, -1, b.lookup, [-1])
        assert_equal(b.lookup(0), ('hello', 0, 0, 3))
        assert_equal(b.lookup(1), ('hello', 1, 0, 3))
        assert_equal(b.lookup(2), ('hello', 2, 0, 3))
        assert_equal(b.lookup(3), ('world', 0, 3, 7))
        assert_equal(b.lookup(4), ('world', 1, 3, 7))
        assert_equal(b.lookup(5), ('world', 2, 3, 7))
        assert_equal(b.lookup(6), ('world', 3, 3, 7))
        assert_raises(KeyError, 7, b.lookup, [7])
        assert_raises(KeyError, 8, b.lookup, [8])


class CodeMapSuite(Suite):

    def test_no_file(self):
        cm = CM()
        assert_raises(KeyError, -1, cm.lookup, [Span(-1, -1, None)])
        assert_raises(KeyError, 0, cm.lookup, [Span(0, 0, None)])

    def test_one_empty_file(self):
        cm = CM()
        cm.add('file1', '')
        # text becomes '\n', with two valid locations: at the \n and after.
        assert_raises(KeyError, -1, cm.lookup, [Span(-1, -1, None)])
        cm.lookup(Span(0, 0, None))
        cm.lookup(Span(0, 1, None))
        cm.lookup(Span(1, 1, None))
        assert_raises(KeyError, 2, cm.lookup, [Span(2, 2, None)])

    def test_two_empty_files(self):
        cm = CM()
        cm.add('file1', '')
        cm.add('file2', '')
        assert_raises(KeyError, -1, cm.lookup, [Span(-1, -1, None)])
        assert_equal(cm.lookup(Span(0, 0, None)), HumanLocation('file1', 1, 1, 0, [''], 0))
        assert_equal(cm.lookup(Span(0, 1, None)), HumanLocation('file1', 1, 1, 1, [''], 1))
        assert_equal(cm.lookup(Span(1, 1, None)), HumanLocation('file1', 2, 1, 0, [''], 0))
        assert_raises(KeyError, Span(1, 2, None), cm.lookup, [Span(1, 2, None)])
        assert_equal(cm.lookup(Span(2, 2, None)), HumanLocation('file2', 1, 1, 0, [''], 0))
        assert_equal(cm.lookup(Span(2, 3, None)), HumanLocation('file2', 1, 1, 1, [''], 1))
        assert_equal(cm.lookup(Span(3, 3, None)), HumanLocation('file2', 2, 1, 0, [''], 0))
        assert_raises(KeyError, 4, cm.lookup, [Span(4, 4, None)])

    def test_multiline(self):
        cm = CM()
        #                012 345678 901 2
        cm.add('file1', 'ab\ncdefg\nhi\n')

        assert_equal(cm.lookup(Span(0, 0, None)), HumanLocation('file1', 1, 1, 0, ['ab'], 0))
        assert_equal(cm.lookup(Span(2, 2, None)), HumanLocation('file1', 1, 3, 0, ['ab'], 2))
        assert_equal(cm.lookup(Span(3, 3, None)), HumanLocation('file1', 2, 1, 0, ['cdefg'], 0))
        assert_equal(cm.lookup(Span(8, 8, None)), HumanLocation('file1', 2, 6, 0, ['cdefg'], 5))
        assert_equal(cm.lookup(Span(9, 9, None)), HumanLocation('file1', 3, 1, 0, ['hi'], 0))
        assert_equal(cm.lookup(Span(11, 11, None)), HumanLocation('file1', 3, 3, 0, ['hi'], 2))
        assert_equal(cm.lookup(Span(12, 12, None)), HumanLocation('file1', 4, 1, 0, [''], 0))

        assert_equal(cm.lookup(Span(0, 1, None)), HumanLocation('file1', 1, 1, 1, ['ab'], 1))
        assert_equal(cm.lookup(Span(2, 3, None)), HumanLocation('file1', 1, 3, 1, ['ab'], 3))
        assert_equal(cm.lookup(Span(3, 4, None)), HumanLocation('file1', 2, 1, 1, ['cdefg'], 1))
        assert_equal(cm.lookup(Span(8, 9, None)), HumanLocation('file1', 2, 6, 1, ['cdefg'], 6))
        assert_equal(cm.lookup(Span(9, 10, None)), HumanLocation('file1', 3, 1, 1, ['hi'], 1))
        assert_equal(cm.lookup(Span(11, 12, None)), HumanLocation('file1', 3, 3, 1, ['hi'], 3))

        assert_equal(cm.lookup(Span(0, 3, None)), HumanLocation('file1', 1, 1, 3, ['ab'], 3))
        assert_equal(cm.lookup(Span(3, 9, None)), HumanLocation('file1', 2, 1, 6, ['cdefg'], 6))
        assert_equal(cm.lookup(Span(9, 12, None)), HumanLocation('file1', 3, 1, 3, ['hi'], 3))

        assert_equal(cm.lookup(Span(2, 4, None)), HumanLocation('file1', 1, 3, 2, ['ab', 'cdefg'], 1))
        assert_equal(cm.lookup(Span(8, 10, None)), HumanLocation('file1', 2, 6, 2, ['cdefg', 'hi'], 1))
        assert_equal(cm.lookup(Span(0, 12, None)), HumanLocation('file1', 1, 1, 12, ['ab', 'cdefg', 'hi'], 3))

    def test_caret(self):
        capture = StringIO()
        cm = CodeMap(FileErrorStream(capture))
        #                     0123 4567 8901 2
        cm.add('test_caret', 'abc\ndef\nghi\n')

        cm.error(Span(4, 4, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:2:1: error: xyzzy\ndef\n^\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(4, 5, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:2:1: error: xyzzy\ndef\n^\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(4, 6, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:2:1: error: xyzzy\ndef\n^~\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(4, 7, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:2:1: error: xyzzy\ndef\n^~~\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(4, 8, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:2:1: error: xyzzy\ndef\n^~~~\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(4, 9, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:2:1: error: xyzzy\ndef\n^~~~ ...\nghi\n~\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(4, 10, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:2:1: error: xyzzy\ndef\n^~~~ ...\nghi\n~~\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(5, 5, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:2:2: error: xyzzy\ndef\n ^\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(5, 6, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:2:2: error: xyzzy\ndef\n ^\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(5, 7, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:2:2: error: xyzzy\ndef\n ^~\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(5, 8, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:2:2: error: xyzzy\ndef\n ^~~\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(5, 9, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:2:2: error: xyzzy\ndef\n ^~~ ...\nghi\n~\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(5, 10, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:2:2: error: xyzzy\ndef\n ^~~ ...\nghi\n~~\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(6, 6, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:2:3: error: xyzzy\ndef\n  ^\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(6, 7, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:2:3: error: xyzzy\ndef\n  ^\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(6, 8, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:2:3: error: xyzzy\ndef\n  ^~\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(6, 9, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:2:3: error: xyzzy\ndef\n  ^~ ...\nghi\n~\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(6, 10, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:2:3: error: xyzzy\ndef\n  ^~ ...\nghi\n~~\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(7, 7, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:2:4: error: xyzzy\ndef\n   ^\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(7, 8, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:2:4: error: xyzzy\ndef\n   ^\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(7, 9, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:2:4: error: xyzzy\ndef\n   ^ ...\nghi\n~\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(7, 10, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:2:4: error: xyzzy\ndef\n   ^ ...\nghi\n~~\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(8, 8, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:3:1: error: xyzzy\nghi\n^\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(11, 11, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:3:4: error: xyzzy\nghi\n   ^\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(11, 12, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:3:4: error: xyzzy\nghi\n   ^\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(12, 12, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:4:1: error: xyzzy\n\n^\n')
        capture.seek(0)
        capture.truncate()

        cm.error(Span(0, 12, None), 'xyzzy')
        assert_equal(capture.getvalue(), 'test_caret:1:1: error: xyzzy\nabc\n^~~~ ...\n(1 lines omitted)\nghi\n~~~~\n')
        capture.seek(0)
        capture.truncate()

    def test_context(self):
        capture = StringIO()
        cm = CodeMap(FileErrorStream(capture))
        #               0         1
        #               0123456789012
        cm.add('ctx1', 'import ctx2')
        #               1      2         3
        #               3456789012345678901234567
        cm.add('ctx2', 'import alt1, main, alt2')
        #               3 4
        #               890123
        cm.add('main', 'oops')
        #               4
        #               45678
        cm.add('alt1', 'foo')
        #               45
        #               90123
        cm.add('alt2', 'bar')

        cm.warning(Span(38, 42, None), 'xyzzy',
                option='-Wfoo',
                pre=[(Span(7, 11, None), 'from here'), (Span(26, 30, None), 'from there')],
                post=[(Span(44, 47, None), 'try this'), (Span(49, 52, None), 'try that')])
        expected = '''\
ctx1:1:8: note: from here
import ctx2
       ^~~~
ctx2:1:14: note: from there
import alt1, main, alt2
             ^~~~
main:1:1: warning: xyzzy [-Wfoo]
oops
^~~~
alt1:1:1: note: try this
foo
^~~
alt2:1:1: note: try that
bar
^~~
'''
        assert_equal(capture.getvalue(), expected)
