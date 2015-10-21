"""Special test that output won't hang"""

import signal
import sys

from mypy.myunit import Suite


class OutputSuite(Suite):
    def test_output(self):
        # This test should finish very quickly, but it may
        # hang indefinitely if stdout is redirected to a blocking pipe.
        signal.alarm(5)
        b = '\r' * 1024
        for _ in range(64):
            sys.stdout.write(b)
        sys.stdout.write('\n')
        signal.alarm(0)
