#!/usr/bin/env python

import sys, os
import unittest
import subprocess

sys.path.append(os.path.join("..", "lib"))
import utils

class TestInstalliPython(utils.ConfigInstallTest):
    def test_install(self):
        Home = os.environ["HOME"]
        self.assertEqual(subprocess.check_output(
            ["zsh", "--login", "-c", "which python"]).decode().strip(),
                         os.path.join(Home, "Python3/bin/python"))
        self.assertEqual(subprocess.check_output(
            ["zsh", "--login", "-c", "which python2"]).decode().strip(),
                         os.path.join(Home, "Python2/bin/python2"))
        self.assertEqual(subprocess.call(
            ["zsh", "--login", "-c", 'python -c "import matplotlib"']),
                         0)

if __name__ == '__main__':
    unittest.main()
