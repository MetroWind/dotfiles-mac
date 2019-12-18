#!/usr/bin/env python

import sys, os
import unittest
import subprocess
import tempfile

sys.path.append(os.path.join("..", "lib"))
import utils

class TestInstalliPython(utils.ConfigInstallTest):
    def test_install(self):
        with tempfile.TemporaryDirectory() as TempDir:
            Source = os.path.join(TempDir.name, "test.rs")
            Bin = os.path.join(TempDir.name, "test")
            with open(Source, 'w') as f:
                f.write("""fn main()
{
    println!("aaa");
}
""")
            subprocess.check_call("zsh", "--login", "-c",
                                  "rustc -o {} {}".format(Bin, Source))
            Output = subprocess.check_output(Bin)
            self.assertEqual(Output.decode().strip(), "aaa")

if __name__ == '__main__':
    unittest.main()
