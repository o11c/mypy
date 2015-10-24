"""Just a compatibility shim, really uses unittest now."""

import importlib
import os
import tempfile
import unittest

from typing import List, Callable


"""
Public interfaces to this module:
    - python -m mypy.myunit and scripts/myunit.
    - TestCase can be subclassed:
        - TestCase.set_up can be overridden.
        - TestCase.tear_down can be overridden.
        - TestCase.run can be overridden.
    - Suite can be subclassed:
        - Suite.cases can be overridden.
        - Suite.test* functions will be collected otherwise.
        - Suite.set_up can be overridden.
    - SkipTestCaseException can be raised within a test to cause a skip.
    - AssertionFailure can be raised with a message that will be printed.
    - UPDATE_TESTCASES and APPEND_TESTCASES are mutable globals.
    - assert_equal can be used to fail.
    - assert_true can be used to fail.
    - assert_false can be used to fail.

All other APIs are private, in particular:
    - Suite being able to contain other Suite is not public.
    - All the details of how tests are actually collected and run.

Interface expected by `unittest`:
    - within a module:
        - tests are collected *only* from unittest.TestCase subclasses.
        - the result is then passed to `load_tests` if it exists, else `unittest.TestSuite`.
    - within a unittest.TestCase subclass:
        - names starting with `test` are collected if they are callable.
            - this uses `dir` and `getattr` on the class.
        - else `runTest` if it exists, regardless of what it is!
        - the class is "constructed" with each name as the sole argument.
            - sneaky metaclass stuff can be done.
            - the result must be callable.
            - the result must *not* be a subclass of unittest.TestCase or TEstSuite.

Compatibility shim:
    - myunit.Suite is a subclass of unittest.TestCase (NOT unittest.TestSuite)
    - the legacy class is instantiated to call init() and cases()
    - the results are then wrapped and assigned as class attributes
        - the wrapper must return a callable object, not a class.
            - usually it is an *instance* of unittest.TestCase or unittest.TestSuite
        - the input is always myunit.TestCase
            - myunit.Suite.add_test is never called with a tuple[str, callable]
            - myunit.Suite.add_test is only called with a tuple[str, Suite] in ListSuite
            - make myunit.TestCase a subclass of unittest.TestCase
"""


# TODO remove global state
APPEND_TESTCASES = os.getenv('MYPY_APPEND_TESTCASES', '')
UPDATE_TESTCASES = bool(APPEND_TESTCASES or os.getenv('MYPY_UPDATE_TESTCASES'))


class AssertionFailure(AssertionError):
    """Exception used to signal skipped test cases."""
    def __init__(self, s: str = None) -> None:
        if s:
            super().__init__(s)
        else:
            super().__init__()


class SkipTestCaseException(unittest.SkipTest): pass


def assert_true(b: bool, msg: str = None) -> None:
    if not b:
        raise AssertionFailure(msg)


def assert_false(b: bool, msg: str = None) -> None:
    if b:
        raise AssertionFailure(msg)


def good_repr(obj: object) -> str:
    if isinstance(obj, str):
        if obj.count('\n') > 1:
            bits = ["'''\\"]
            for line in obj.split('\n'):
                # force repr to use ' not ", then cut it off
                bits.append(repr('"' + line)[2:-1])
            bits[-1] += "'''"
            return '\n'.join(bits)
    return repr(obj)


def assert_equal(a: object, b: object, fmt: str = '{} != {}') -> None:
    if a != b:
        raise AssertionFailure(fmt.format(good_repr(a), good_repr(b)))


class TestCase:
    def __init__(self, name: str, suite: 'Suite' = None,
                 func: Callable[[], None] = None) -> None:
        self.func = func
        self.name = name
        self.suite = suite
        self.old_cwd = None  # type: str
        self.tmpdir = None  # type: tempfile.TemporaryDirectory

    def run(self) -> None:
        if self.func:
            self.func()

    def set_up(self) -> None:
        self.old_cwd = os.getcwd()
        self.tmpdir = tempfile.TemporaryDirectory(prefix='mypy-test-',
                dir=os.path.abspath('tmp-test-dirs'))
        os.chdir(self.tmpdir.name)
        os.mkdir('tmp')
        if self.suite:
            self.suite.set_up()

    def tear_down(self) -> None:
        if self.suite:
            self.suite.tear_down()
        os.chdir(self.old_cwd)
        self.tmpdir.cleanup()
        self.old_cwd = None
        self.tmpdir = None


class SuiteMeta(type):
    def __new__(mcls, name, bases, dct) -> type:
        cls = type.__new__(mcls, name, bases, dct)
        if bases:
            cls2 = MyunitShimMeta(name, suite_class=cls)
            mod = importlib.import_module(cls.__module__)
            setattr(mod, cls2.__name__, cls2)
        return cls

    def __init__(cls, name, bases, dct, **kwargs) -> None:
        pass


class MyunitShimMeta(type):
    def __new__(mcls, name, *, suite_class):
        name += 'Wrapper'
        bases = (SuiteWrapper,)
        dct = {}
        cls = type.__new__(mcls, name, bases, dct)
        cls._suite_class = suite_class
        cls._suite_instance = None
        return cls

    def __init__(cls, *args, **kwargs):
        pass

    def __getattr__(cls, name):
        if cls._suite_class is None:
            raise AttributeError(name)
        cls._finish()
        return getattr(cls, name)

    def __dir__(cls):
        cls._finish()
        # type.__dir__ only exists in 3.3+
        return list(cls.__dict__.keys())

    def _finish(cls):
        if cls._suite_class is not None:
            cls._suite_instance = suite = cls._suite_class()
            cases = suite.cases()
            for c in cases:
                assert c.name.startswith('test')
                setattr(cls, c.name, make_wrapper(c))
            cls._suite_class = None


def make_wrapper(case):
    def do_set_up():
        case.set_up()

    def do_tear_down():
        case.tear_down()

    def do_run(self):
        case.run()

    do_run.do_set_up = do_set_up
    do_run.do_tear_down = do_tear_down
    do_run.__name__ = case.name
    return do_run


class SuiteWrapper(unittest.TestCase):
    def setUp(self):
        getattr(self, self._testMethodName).do_set_up()

    def tearDown(self):
        getattr(self, self._testMethodName).do_tear_down()


class Suite(metaclass=SuiteMeta):
    def __init__(self) -> None:
        self._test_cases = []  # type: List[TestCase]
        self.init()

    def set_up(self) -> None:
        pass

    def tear_down(self) -> None:
        pass

    def init(self) -> None:
        for m in dir(self):
            if m.startswith('test'):
                t = getattr(self, m)
                self.add_test(TestCase(m, self, t))

    def add_test(self, test: TestCase) -> None:
        self._test_cases.append(test)

    def cases(self) -> List[TestCase]:
        return self._test_cases[:]

    def skip(self) -> None:
        raise SkipTestCaseException()
