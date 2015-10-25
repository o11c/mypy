#!/usr/bin/env python

import glob
import os
import os.path
import shutil
import sys

from setuptools import setup, find_packages
from mypy.version import __version__

if sys.version_info[0] != 2:
    from mypy import git
    git.verify_git_integrity_or_abort(".")

version = __version__
description = 'Optional static typing for Python'
long_description = '''
Mypy -- Optional Static Typing for Python
=========================================

Add type annotations to your Python programs, and use mypy to type
check them.  Mypy is essentially a Python linter on steroids, and it
can catch many programming errors by analyzing your program, without
actually having to run it.  Mypy has a powerful type system with
features such as type inference, gradual typing, generics and union
types.

In Python2 mode, this only installs the `typing` module and the
`mypy.codec` package.
'''.lstrip()


classifiers = [
    'Development Status :: 3 - Alpha',
    'Environment :: Console',
    'Intended Audience :: Developers',
    'License :: OSI Approved :: MIT License',
    'Operating System :: POSIX',
    'Programming Language :: Python :: 2.7',
    'Programming Language :: Python :: 3.2',
    'Programming Language :: Python :: 3.3',
    'Programming Language :: Python :: 3.4',
    'Programming Language :: Python :: 3.5',
    'Topic :: Software Development',
]

if sys.version_info[0] == 2:
    packages = ['mypy.codec', 'mypy.codec.test']
else:
    packages = find_packages(exclude=['pinfer'])


def find_data_dirs(base, globs, cut):
    """Find all interesting data files, for setup(data_files=)

    Arguments:
      root:  The directory to search in.
      globs: A list of glob patterns to accept files.
    """

    rv_dirs = [os.path.relpath(root, cut) for root, dirs, files in os.walk(base)]
    rv = []
    for rv_dir in rv_dirs:
        for pat in globs:
            rv.append(os.path.join(rv_dir, pat))
    return rv

if sys.version_info[0] == 2:
    package_data = {}
else:
    package_data = {
        'mypy': find_data_dirs('mypy/data', ['*.pyi'], 'mypy')
        + find_data_dirs('mypy/data/typeshed', ['*.pyi'], 'mypy')
        + [
            'xml/*.xsd',
            'xml/*.xslt',
            'xml/*.css',
        ],
        'mypy.test': [
            'data/*.test',
            'data/fixtures/*.pyi',
            'data/lib-stub/*.pyi',
        ],
    }

if sys.version_info[0] == 2:
    libtyping_dir = 'lib-typing/2.7'
else:
    libtyping_dir = 'lib-typing/3.2'

if sys.version_info[0] == 2:
    py_modules = ['typing', 'test_typing', 'mypy.__init__', 'mypy.version']
else:
    py_modules = ['typing', 'test_typing']

if sys.version_info[0] == 2:
    scripts = []
else:
    scripts = ['scripts/mypy']

# Ensure that we don't install stale python3 files under python2 or vice versa.
shutil.rmtree('build/lib', ignore_errors=True)
assert not os.path.exists('build/lib')

install_requires = [
    'flake8',
    'pytest',
]
if sys.version_info[:2] < (3, 4):
    install_requires.append('enum34')

setup(
    name='mypy-lang',
    version=version,
    description=description,
    long_description=long_description,
    author='Jukka Lehtosalo',
    author_email='jukka.lehtosalo@iki.fi',
    maintainer='Ben Longbons',
    maintainer_email='brlongbons@gmail.com',
    url='https://github.com/o11c/mypy',
    license='MIT License',
    platforms=['POSIX'],
    package_dir={'': libtyping_dir, 'mypy': 'mypy'},
    py_modules=py_modules,
    packages=packages,
    scripts=scripts,
    package_data=package_data,
    classifiers=classifiers,
    zip_safe=False,
    install_requires=install_requires,
    # TODO entry_points=
)
