#!/usr/bin/env python3
"""Mypy test runner."""

if False:
    import typing

if True:
    # When this is run as a script, `typing` is not available yet.
    import sys
    from os.path import (
        basename,
        dirname,
        isabs,
        isdir,
        join,
        realpath,
        relpath,
        splitext,
    )
    if realpath(sys.path[0]) == dirname(realpath(__file__)):
        del sys.path[0]

    if isdir('lib-typing/3.2'):
        sys.path[0:0] = ['lib-typing/3.2']
    # Now `typing` is available.


from typing import Dict, List, Optional, Set

import mypy
from mypy.build import is_installed, get_versions
from mypy.syntax.dialect import default_implementation
from mypy.waiter import Waiter, LazySubprocess
from mypy import git

import itertools
import os


# Allow this to be symlinked to support running an installed version.
SOURCE_DIR = dirname(realpath(__file__))
IMPLEMENTATION = default_implementation()
DIALECT = IMPLEMENTATION.base_dialect
print('Running tests for %r' % DIALECT)


# Ideally, all tests would be `discover`able so that they can be driven
# (and parallelized) by an external test driver.

class Driver:

    def __init__(self, whitelist: List[str], blacklist: List[str],
            arglist: List[str], verbosity: int, xfail: List[str]) -> None:
        self.whitelist = whitelist or xfail
        self.blacklist = blacklist
        self.arglist = arglist
        self.verbosity = verbosity
        self.waiter = Waiter(verbosity=verbosity, xfail=xfail)
        self.versions = get_versions(DIALECT)
        self.cwd = os.getcwd()
        self.env = dict(os.environ)
        self.count = 0

    def prepend_path(self, name: str, paths: List[str]) -> None:
        assert not isinstance(paths, str)
        old_val = self.env.get(name)
        paths = [p for p in paths if isdir(p)]
        if not paths:
            return
        if old_val is not None:
            new_val = ':'.join(itertools.chain(paths, [old_val]))
        else:
            new_val = ':'.join(paths)
        self.env[name] = new_val

    def allow(self, name: str) -> bool:
        self.count += 1
        if any(f in name for f in self.whitelist):
            if not any(f in name for f in self.blacklist):
                if self.verbosity >= 2:
                    print('SELECT   #%d %s' % (len(self.waiter.queue), name))
                return True
        if self.verbosity >= 3:
            print('OMIT     %s' % name)
        return False

    def add_mypy(self, name: str, *args: str, cwd: Optional[str] = None) -> None:
        name = 'check %s' % name
        if not self.allow(name):
            return
        largs = list(args)
        largs[0:0] = ['mypy']
        env = self.env
        self.waiter.add(LazySubprocess(name, largs, cwd=cwd, env=env))

    def add_python(self, name: str, *args: str, cwd: Optional[str] = None) -> None:
        name = 'run %s' % name
        if not self.allow(name):
            return
        largs = list(args)
        largs[0:0] = [sys.executable]
        env = self.env
        self.waiter.add(LazySubprocess(name, largs, cwd=cwd, env=env))

    def add_mypy_mod(self, name: str, *args: str, cwd: Optional[str] = None) -> None:
        name = 'check %s' % name
        if not self.allow(name):
            return
        largs = list(args)
        largs[0:0] = ['mypy', '-m']
        env = self.env
        self.waiter.add(LazySubprocess(name, largs, cwd=cwd, env=env))

    def add_python_mod(self, name: str, *args: str, cwd: Optional[str] = None) -> None:
        name = 'run %s' % name
        if not self.allow(name):
            return
        largs = list(args)
        largs[0:0] = [sys.executable, '-m']
        env = self.env
        self.waiter.add(LazySubprocess(name, largs, cwd=cwd, env=env))

    def add_mypy_string(self, name: str, *args: str, cwd: Optional[str] = None) -> None:
        name = 'check %s' % name
        if not self.allow(name):
            return
        largs = list(args)
        largs[0:0] = ['mypy', '-c']
        env = self.env
        self.waiter.add(LazySubprocess(name, largs, cwd=cwd, env=env))

    def add_python_string(self, name: str, *args: str, cwd: Optional[str] = None) -> None:
        name = 'run %s' % name
        if not self.allow(name):
            return
        largs = list(args)
        largs[0:0] = [sys.executable, '-c']
        env = self.env
        self.waiter.add(LazySubprocess(name, largs, cwd=cwd, env=env))

    def add_python2(self, name: str, *args: str, cwd: Optional[str] = None) -> None:
        if DIALECT.major != 2:
            return
        name = 'run2 %s' % name
        if not self.allow(name):
            return
        largs = list(args)
        largs[0:0] = [IMPLEMENTATION.executable]
        env = self.env
        self.waiter.add(LazySubprocess(name, largs, cwd=cwd, env=env))

    def add_myunit(self, name: str, *args: str, cwd: Optional[str] = None) -> None:
        name = 'run %s' % name
        if not self.allow(name):
            return
        largs = list(args)
        largs[0:0] = ['myunit']
        env = self.env
        self.waiter.add(LazySubprocess(name, largs, cwd=cwd, env=env))

    def add_flake8(self, name: str, file: str, cwd: Optional[str] = None) -> None:
        name = 'lint %s' % name
        if not self.allow(name):
            return
        largs = ['flake8', file]
        env = self.env
        self.waiter.add(LazySubprocess(name, largs, cwd=cwd, env=env))

    def list_tasks(self) -> None:
        for id, task in enumerate(self.waiter.queue):
            print('{id}:{task}'.format(id=id, task=task.name))


def add_basic(driver: Driver) -> None:
    if False:
        driver.add_mypy('file setup.py', join(SOURCE_DIR, 'setup.py'))
    driver.add_flake8('file setup.py', join(SOURCE_DIR, 'setup.py'))
    if DIALECT.major == 2:
        return
    driver.add_mypy('file runtests.py', join(SOURCE_DIR, 'runtests.py'))
    driver.add_flake8('file runtests.py', join(SOURCE_DIR, 'runtests.py'))
    driver.add_mypy('legacy entry script', join(SOURCE_DIR, 'scripts/mypy'))
    driver.add_flake8('legacy entry script', join(SOURCE_DIR, 'scripts/mypy'))
    driver.add_mypy('legacy myunit script', join(SOURCE_DIR, 'scripts/myunit'))
    driver.add_flake8('legacy myunit script', join(SOURCE_DIR, 'scripts/myunit'))
    driver.add_mypy_mod('entry mod mypy', 'mypy')
    driver.add_mypy_mod('entry mod mypy.stubgen', 'mypy.stubgen')
    driver.add_mypy_mod('entry mod mypy.myunit', 'mypy.myunit')


def find_files(base: str, prefix: str = '', suffix: str = '') -> List[str]:
    base = join(SOURCE_DIR, base)
    return [join(root, f)
            for root, dirs, files in os.walk(base)
            for f in files
            if f.startswith(prefix) and f.endswith(suffix)]


def file_to_module(file: str, ignore: str = '') -> str:
    file = relpath(file, join(SOURCE_DIR, ignore))
    if file.startswith('lib-typing'):
        file = file[len('lib-typing/x.y/'):]
    rv = splitext(file)[0].replace(os.sep, '.')
    if rv.endswith('.__init__'):
        rv = rv[:-len('.__init__')]
    return rv


def add_imports(driver: Driver) -> None:
    if DIALECT.major != 2:
        dirs = ['mypy', 'lib-typing/3.2']
    else:
        dirs = ['mypy/codec', 'lib-typing/2.7']
    # Make sure each module can be imported originally.
    # There is currently a bug in mypy where a module can pass typecheck
    # because of *implicit* imports from other modules.
    for d in dirs:
        for f in find_files(d, suffix='.py'):
            assert 'setup' not in f, 'd: %s, f: %s' % (d, f)
            mod = file_to_module(f)
            driver.add_mypy_string('import %s' % mod, 'import %s' % mod)
            if not mod.endswith('.__main__'):
                driver.add_python_string('import %s' % mod, 'import %s' % mod)
            driver.add_flake8('module %s' % mod, f)


def add_myunit(driver: Driver) -> None:
    for f in find_files('mypy', prefix='test', suffix='.py'):
        mod = file_to_module(f)
        if '.codec.test.' in mod:
            # myunit is Python3 only.
            if DIALECT.major != 2:
                driver.add_python_mod('unittest %s' % mod, 'unittest', mod)
            else:
                driver.add_python2('unittest %s' % mod, '-m', 'unittest', mod)
        elif mod == 'mypy.test.testpythoneval':
            # Run Python evaluation integration tests separetely since they are much slower
            # than proper unit tests.
            driver.add_myunit('eval-test %s' % mod, '-m', mod, *driver.arglist)
        else:
            driver.add_myunit('unit-test %s' % mod, '-m', mod, *driver.arglist)


def add_stubs(driver: Driver) -> None:
    # Only test each module once, for the latest Python version supported.
    # The third-party stub modules will only be used if it is not in the version.
    seen = set()  # type: Set[str]
    for stub_type in [
            'stubs-override',
            'typeshed/builtins',
            'typeshed/stdlib',
            'typeshed/third_party',
            'stubs-auto',
    ]:
        for version in driver.versions:
            stubdir = join('mypy/data', stub_type, version)
            for f in find_files(stubdir, suffix='.pyi'):
                module = file_to_module(f, stubdir)
                if module not in seen:
                    seen.add(module)
                    driver.add_mypy_string(
                        'stub (%s) module %s' % (join(stub_type, version), module),
                        'import typing, %s' % module)


def add_libpython(driver: Driver) -> None:
    seen = set()  # type: Set[str]
    for version in driver.versions:
        libpython_dir = join('lib-python', version)
        for f in find_files(libpython_dir, prefix='test_', suffix='.pyi'):
            module = file_to_module(f, libpython_dir)
            if module not in seen:
                seen.add(module)
                driver.add_mypy_mod(
                    'libpython (%s) module %s' % (version, module),
                    module,
                    cwd=join(SOURCE_DIR, libpython_dir))


def add_samples(driver: Driver) -> None:
    for f in find_files('samples', suffix='.py'):
        if 'codec' in f:
            cwd, bf = dirname(f), basename(f)
            bf = bf[:-len('.py')]
            f = relpath(f, SOURCE_DIR)
            driver.add_mypy_string('codec file %s' % f,
                    'import mypy.codec.register, %s' % bf,
                    cwd=cwd)
        elif DIALECT.major != 2:
            f = relpath(f, SOURCE_DIR)
            driver.add_mypy('file %s' % f, f, cwd=SOURCE_DIR)


def usage(status: int) -> None:
    print('Usage: %s [-h | -v | -q | [-x] FILTER | -a ARG] ... [-- FILTER ...]' % sys.argv[0])
    print()
    print('Run mypy tests. If given no arguments, run all tests.')
    print()
    print('Examples:')
    print('  %s unit-test  (run unit tests only)' % sys.argv[0])
    print('  %s unit-test -a "*tuple*"' % sys.argv[0])
    print('       (run all unit tests with "tuple" in test name)')
    print()
    print('Options:')
    print('  -h, --help             show this help')
    print('  -v, --verbose          increase driver verbosity')
    print('  -q, --quiet            decrease driver verbosity')
    print('  -a, --argument ARG     pass an argument to myunit tasks')
    print('                         (-v: verbose; glob pattern: filter by test name)')
    print('  -l, --list             list included tasks (after filtering) and exit')
    print('  FILTER                 include tasks matching FILTER')
    print('  -x, --exclude FILTER   exclude tasks matching FILTER')
    print('  --                     treat all remaning arguments as positional')
    sys.exit(status)


def sanity() -> None:
    paths = os.getenv('PYTHONPATH')
    if paths is None:
        return
    failed = False
    for p in paths.split(os.pathsep):
        if not isabs(p):
            print('Relative PYTHONPATH entry %r' % p)
            failed = True
    if failed:
        print('Please use absolute so that chdir() tests can work.')
        print('Cowardly refusing to continue.')
        sys.exit(1)


def main() -> None:
    sanity()

    verbosity = 0
    whitelist = []  # type: List[str]
    blacklist = []  # type: List[str]
    arglist = []  # type: List[str]
    list_only = False
    xfail_only = False
    dirty_stubs = False

    allow_opts = True
    curlist = whitelist
    args = sys.argv[1:]
    for i, a in enumerate(args, 1):
        if curlist is not arglist and allow_opts and a.startswith('@'):
            with open(a[1:]) as f:
                args[i:i] = [x[:-1] for x in f]
            continue
        if curlist is not arglist and allow_opts and a.startswith('-'):
            if curlist is not whitelist:
                break
            if a == '--':
                allow_opts = False
            elif a == '-v' or a == '--verbose':
                verbosity += 1
            elif a == '-q' or a == '--quiet':
                verbosity -= 1
            elif a == '-x' or a == '--exclude':
                curlist = blacklist
            elif a == '-a' or a == '--argument':
                curlist = arglist
            elif a == '-l' or a == '--list':
                list_only = True
            elif a == '--xfail':
                xfail_only = True
            elif a == '-f' or a == '--dirty-stubs':
                dirty_stubs = True
            elif a == '-h' or a == '--help':
                usage(0)
            else:
                usage(1)
        else:
            curlist.append(a)
            curlist = whitelist
    if curlist is blacklist:
        sys.exit('-x must be followed by a filter')
    if curlist is arglist:
        sys.exit('-a must be followed by an argument')
    if xfail_only and whitelist:
        sys.exit('Sorry, --xfail can only be used with blacklist right now')
    # empty string is a substring of all names
    if not xfail_only and not whitelist:
        whitelist.append('')

    driver = Driver(whitelist=whitelist, blacklist=blacklist, arglist=arglist,
            verbosity=verbosity, xfail=[
                'check import test_typing',
                'lint module test_typing',
                'lint module typing',
                'check stub (typeshed/third_party/2.7) module sqlalchemy',
                'check stub (typeshed/third_party/2.7) module sqlalchemy.inspection',
                'check stub (typeshed/third_party/2.7) module sqlalchemy.schema',
                'check stub (typeshed/third_party/2.7) module sqlalchemy.types',
                'check stub (typeshed/third_party/2.7) module sqlalchemy.pool',
                'check stub (typeshed/third_party/2.7) module sqlalchemy.databases.mysql',
                'check stub (typeshed/third_party/2.7) module sqlalchemy.exc',
                'check stub (typeshed/third_party/2.7) module sqlalchemy.databases',
                'check stub (typeshed/third_party/2.7) module sqlalchemy.dialects.mysql',
                'check stub (typeshed/third_party/2.7) module sqlalchemy.dialects.mysql.base',
                'check stub (typeshed/third_party/2.7) module sqlalchemy.dialects',
                'check stub (typeshed/third_party/2.7) module sqlalchemy.util',
                'check stub (typeshed/third_party/2.7) module sqlalchemy.sql',
                'check stub (typeshed/third_party/2.7) module sqlalchemy.sql.expression',
                'check stub (typeshed/third_party/2.7) module sqlalchemy.sql.visitors',
                'check stub (typeshed/third_party/2.7) module sqlalchemy.util.langhelpers',
                'check stub (typeshed/third_party/2.7) module sqlalchemy.util._collections',
                'check stub (typeshed/third_party/2.7) module sqlalchemy.util.deprecations',
                'check stub (typeshed/third_party/2.7) module sqlalchemy.util.compat',
                'check stub (typeshed/third_party/2.7) module sqlalchemy.orm',
                'check stub (typeshed/third_party/2.7) module sqlalchemy.orm.session',
                'check stub (typeshed/third_party/2.7) module sqlalchemy.engine',
                'check stub (typeshed/third_party/2.7) module sqlalchemy.engine.url',
                'check stub (typeshed/third_party/2.7) module sqlalchemy.engine.strategies',

                # Stubs correct, `import as` bug in mypy - fix in other branch.
                'check import mypy.codec.register',
                'check import mypy.codec.mypy_codec',
                'check import mypy.codec.tokenizer',
                'check import mypy.codec.pytokenize',
                'check import mypy.codec.test.test_function_translation',
                'check stub (typeshed/third_party/2.7) module scribe.scribe',
                'check stub (typeshed/third_party/2.7) module thrift.transport.TSocket',
                'check stub (typeshed/third_party/2.7) module thrift.protocol.TProtocol',
                'check stub (typeshed/third_party/2.7) module thrift.protocol.TBinaryProtocol',
                'check codec file samples/codec/example.py',
            ])

    if not dirty_stubs:
        git.verify_git_integrity_or_abort(SOURCE_DIR)

    # Don't --use-python-path, only make mypy available.
    # Now that we're using setuptools and mypy is in an .egg directory,
    # this won't even catch other modules.
    for p in IMPLEMENTATION.python_path:
        if isdir(join(p, 'mypy')):
            # This must *not* be before stubs.
            driver.prepend_path('MYPYPATH_APPEND', [p])
            break
    else:
        assert False, 'unable to find inferior mypy'
    if not is_installed():
        driver.prepend_path('PATH', [join(driver.cwd, 'scripts')])
        driver.prepend_path('PYTHONPATH', [driver.cwd])
        if DIALECT.major != 2:
            v = '3.2'
        else:
            v = '2.7'
        driver.prepend_path('PYTHONPATH', [join(driver.cwd, 'lib-typing', v)])

    for adder in [
            add_basic,
            add_myunit,
            add_imports,
            add_stubs,
            add_libpython,
            add_samples,
    ]:
        before = driver.count
        adder(driver)
        if DIALECT.major == 2:
            if adder is add_libpython:
                continue
        assert driver.count != before, 'no tasks in %s' % adder.__name__

    if not list_only:
        driver.waiter.run()
    else:
        driver.list_tasks()


if __name__ == '__main__':
    main()
