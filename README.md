Mypy: Optional Static Typing for Python
=======================================

[![Build Status](https://travis-ci.org/JukkaL/mypy.svg)](https://travis-ci.org/JukkaL/mypy)


What is mypy?
-------------

Mypy is an optional static type checker for Python.  You can add type
hints to your Python programs using the upcoming standard for type
annotations introduced in Python 3.5 beta 1 (PEP 484), and use mypy to
type check them statically. Find bugs in your programs without even
running them!

The type annotation notation has also been backported to earlier
Python 3.x versions.  Mypy programs are valid Python 3.x and you use a
normal Python interpreter to run them.  There is essentially no
performance overhead when using mypy, since mypy does not introduce
runtime type checking.

You can mix dynamic and static typing in your programs. You can always
fall back to dynamic typing when static typing is not convenient, such
as for legacy code.

Here is a small example to whet your appetite:

```
from typing import Iterator

def fib(n: int) -> Iterator[int]:
    a, b = 0, 1
    while a < n:
        yield a
        a, b = b, a + b
```

Mypy is in development; some features are missing and there are bugs.
See 'Development status' below.


Requirements
------------

You need Python 3.2 or later to run mypy.  You can have multiple Python
versions (2.x and 3.x) installed on the same system without problems.

In Ubuntu, Mint and Debian you can install Python 3 like this:

    $ sudo apt-get install python3 python3-pip

For other Linux flavors, OS X and Windows, packages are available at

  http://www.python.org/getit/


Quick start
-----------

Mypy can be installed using pip:

    $ pip3 install mypy-lang

*Note that the package name is `mypy-lang`, not `mypy`.*

Now, if Python on your system is configured properly (else see
"Troubleshooting" below), you can type-check a program like this:

    $ mypy PROGRAM

You can always use a Python interpreter to run your statically typed
programs, even if they have type errors:

    $ python3 PROGRAM


Web site and documentation
--------------------------

Documentation and additional information is available at the web site:

  http://www.mypy-lang.org/


Troubleshooting
---------------

Depending on your configuration, you may have to run `pip` like
this:

    $ python3 -m pip install mypy-lang

If the `mypy` command isn't found after installation: After either
`pip3 install` or `setup.py install`, the `mypy` script and
dependencies, including the `typing` module, will be installed to
system-dependent locations.  Sometimes the script directory will not
be in `PATH`, and you have to add the target directory to `PATH`
manually or create a symbolic link to the script.  In particular, on
Mac OS X, the script may be installed under `/Library/Frameworks`:

    /Library/Frameworks/Python.framework/Versions/<version>/bin

In Windows, the script is generally installed in
`\PythonNN\Scripts`. So, type check a program like this (replace
`\Python34` with your Python installation path):

    C:\>\Python34\python \Python34\Scripts\mypy PROGRAM


Quick start for contributing to mypy
------------------------------------

If you want to contribute, first clone the mypy git repository:

    $ git clone --recurse-submodules https://github.com/JukkaL/mypy.git

Run the supplied `setup.py` script to install mypy:

    $ python3 setup.py install

Replace `python3` with your Python 3 interpreter.  You may have to do
the above as root. For example, in Ubuntu and Mac OS X:

    $ sudo python3 setup.py install

Now you can use the `mypy` program just as above.  In case of trouble
see "Troubleshooting" above.

The mypy wiki contains some useful information for contributors:

  http://www.mypy-lang.org/wiki/DeveloperGuides

Working with the git version of mypy
------------------------------------

mypy contains a submodule, "typeshed". See http://github.com/python/typeshed.
This submodule contains types for the Python standard library.

Due to the way git submodules work, you'll have to do
```
  git submodule update typeshed
```
whenever you change branches, merge, rebase, or pull.

(It's possible to automate this: Search Google for "git hook update submodule")

Running tests and linting
-------------------------

First install any additional dependencies needed for testing:

    $ pip3 install -r test-requirements.txt

To run all tests, run the script `runtests.py` in the mypy repository:

    $ ./runtests.py

Note that some tests will be disabled for older python versions.

This will run all tests, including integration and regression tests,
and will type check mypy and verify that all stubs are valid. You can also
run unit tests only, which run pretty quickly:

    $ ./runtests.py unit-test

You can run a subset of test suites by passing positive or negative
filters:

    $ ./runtests.py lex parse -x lint -x stub

If you want to run individual unit tests, you can run `myunit` directly, or
pass inferior arguments via `-a`:

    $ scripts/myunit -m mypy.test.testlex -v '*backslash*'
    $ ./runtests.py mypy.test.testlex -a -v -a '*backslash*'

You can also run the type checker for manual testing without
installing anything by setting up the Python module search path suitably:

    $ export PYTHONPATH=$PWD:$PWD/lib-typing/3.2
    $ python<version> -m mypy PROGRAM.py

You can add the entry scripts to PATH for a single python3 version:

    $ export PATH=$PWD/scripts
    $ mypy PROGRAM.py

You can check a module or string instead of a file:

    $ mypy PROGRAM.py
    $ mypy -m MODULE
    $ mypy -c 'import MODULE'

To run the linter:

    $ ./runtests.py lint


Development status
------------------

Mypy is work in progress and is not yet production quality, though
mypy development has been done using mypy for a while!

Here are some of the more significant Python features not supported
right now (but all of these will improve):

 - Python 2.x support needs polish and documentation
 - properties with setters not supported
 - limited metaclass support
 - only a subset of Python standard library modules are supported, and some
   only partially
 - 3rd party module support is limited

The current development focus is to have a good coverage of Python
features and the standard library (both 3.x and 2.7).


Issue tracker
-------------

Please report any bugs and enhancement ideas using the mypy issue
tracker:

  https://github.com/JukkaL/mypy/issues

Feel free to also ask questions on the tracker.


Help wanted
-----------

Any help in testing, development, documentation and other tasks is
highly appreciated and useful to the project.  Contact the developers
to join the project, or just start coding and send pull requests!
There are tasks for contributors of all experience levels.


License
-------

Mypy is licensed under the terms of the MIT License (see the file
LICENSE).
