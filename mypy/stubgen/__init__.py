"""Generator of dynamically typed draft stubs for arbitrary modules.

Basic usage:

  $ python3 -m mypy.stubgen urllib.parse

  => Generate mypy/data/stubs-auto/urllib/parse.pyi.

For C modules, you can get more precise function signatures by parsing .rst (Sphinx)
documentation for extra information. For this, use the --docpath option:

  $ python3 -m mypy.stubgen --docpath <DIR>/Python-3.4.2/Doc/library curses

  => Generate mypy/data/stubs-auto/curses.py.

Note: You should verify the generated stubs manually.

TODO:

 - infer some return types, such as no return statement with value -> None
 - detect 'if PY2 / is_py2' etc. and either preserve those or only include Python 2 or 3 case
 - maybe export more imported names if there is no __all__ (this affects ssl.SSLError, for example)
   - a quick and dirty heuristic would be to turn this on if a module has something like
     'from x import y as _y'
 - we don't seem to always detect properties ('closed' in 'io', for example)
"""

import glob
import imp
import importlib
import os.path
import sys

from typing import Any, Dict

from mypy.syntax.dialect import Dialect, default_dialect, default_implementation
from mypy.stubgen.stubgenc import (
    parse_all_signatures, find_unique_signatures, generate_stub_for_c_module
)
from mypy.stubgen.stubgenpy import (
    generate_stub, load_python2_module_info
)
from mypy.stubgen.stubutil import is_c_module


def generate_stub_for_module(module: str, output_dir: str, quiet: bool = False,
                             add_header: bool = False, sigs: Dict[str, str] = {},
                             class_sigs: Dict[str, str] = {},
                             dialect: Dialect = default_dialect()) -> None:
    if dialect.major == 2:
        module_path, module_all = load_python2_module_info(module)
    else:
        mod = importlib.import_module(module)
        imp.reload(mod)
        if is_c_module(mod):
            target = module.replace('.', '/') + '.pyi'
            target = os.path.join(output_dir, target)
            generate_stub_for_c_module(module_name=module,
                                       target=target,
                                       add_header=add_header,
                                       sigs=sigs,
                                       class_sigs=class_sigs)
            return
        module_path = mod.__file__
        module_all = getattr(mod, '__all__', None)
    target = module.replace('.', '/')
    if os.path.basename(module_path) == '__init__.py':
        target += '/__init__.pyi'
    else:
        target += '.pyi'
    target = os.path.join(output_dir, target)
    generate_stub(module_path, output_dir, module_all,
                  target=target, add_header=add_header, module=module, dialect=dialect)
    if not quiet:
        print('Created %s' % target)


def main():
    out = 'mypy/data/stubs-auto'
    if not os.path.isdir(out):
        raise SystemExit('Directory %r does not exist' % out)
    args = sys.argv[1:]
    sigs = {}
    class_sigs = {}
    dialect = default_implementation().base_dialect
    while args and args[0].startswith('--'):
        if args[0] == '--docpath':
            docpath = args[1]
            args = args[2:]
            all_sigs = []  # type: Any
            all_class_sigs = []  # type: Any
            for path in glob.glob('%s/*.rst' % docpath):
                func_sigs, class_sigs = parse_all_signatures(open(path).readlines())
                all_sigs += func_sigs
                all_class_sigs += class_sigs
            sigs = dict(find_unique_signatures(all_sigs))
            class_sigs = dict(find_unique_signatures(all_class_sigs))
        elif args[0] == '--py2':
            dialect = default_implementation(force_py2=True).base_dialect
        else:
            raise SystemExit('Unrecognized option %s' % args[0])
        args = args[1:]
    if not args:
        usage()
    out = os.path.join(out, dialect.base_version[:3])
    for module in args:
        generate_stub_for_module(module, out, add_header=True, sigs=sigs, class_sigs=class_sigs,
                                 dialect=dialect)


def usage():
    raise SystemExit('usage: python3 -m mypy.stubgen [--docpath path] [--py2] module ...')
