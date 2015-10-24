#!/usr/bin/env python
import os
import subprocess
import sys

assert sys.executable == os.path.join(sys.prefix, 'bin/python')
orig_python = os.readlink(sys.executable)

if sys.version_info[0] != 2:
    print('This is python3! Not doing the hack!')
else:
    if os.path.exists(os.path.join(sys.prefix, 'bin/python3')):
        print('This is python2, but not doing the hack!')
    else:
        print('This is python2, and doing the hack!')
        # pypy installs include/ as a symlink, breaking others.
        # We don't need include/ at all.
        if os.path.islink(os.path.join(sys.prefix, 'include')):
            os.remove(os.path.join(sys.prefix, 'include'))
        subprocess.check_call(['virtualenv', '-p', 'python3', sys.prefix])
        os.remove(sys.executable)
        os.symlink(orig_python, sys.executable)
