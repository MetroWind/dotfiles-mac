#!/usr/bin/env python

import sys, os
import unittest

sys.path.append(os.path.join("..", "lib"))
import utils

class TestInstallFirefox(utils.ConfigInstallTest):
    def test_app_install(self):
        self.assertTrue(os.path.exists("/Applications/Firefox.app"))

if __name__ == '__main__':
    unittest.main()
