#!/usr/bin/env python
# -*- coding: utf-8; -*-

from __future__ import print_function

import sys, os
import unittest
if sys.version_info.major < 3:
    import imp
else:
    import importlib.machinery
    import importlib.util

import install
sys.path.append("lib")
import utils

Log = utils.getLogger(__name__)

def importFile(name, path):
    if sys.version_info.major < 3:
        with open(path, 'r') as f:
            return imp.load_module(name, f, path, (".py", 'r', imp.PY_SOURCE))
    else:
        # This seems to be the only correct way to import an arbitrary file in
        # Python 3.4+. For any other way, the imported module itself is fine,
        # but calling inspect.getsourcefile(<some_name_in_module>) throws an
        # exception...
        Loader = importlib.machinery.SourceFileLoader(name, path)
        Spec = importlib.util.spec_from_loader(Loader.name, Loader)
        Module = importlib.util.module_from_spec(Spec)
        Loader.exec_module(Module)
        sys.modules[name] = Module
        return Module

def getTestClasses(os_type):
    TestClasses = []
    for Pkg in ["bootstrap",] + install.AllPkgs:
        TestFile = os.path.join(Pkg, os_type.TestName)
        if os.path.exists(TestFile):
            Log.info("Test found for {}.".format(Pkg))
            Mod = importFile("test-" + Pkg, os.path.join(Pkg, os_type.TestName))
            for Name in dir(Mod):
                Class = getattr(Mod, Name)
                if type(Class) == type and issubclass(Class, unittest.TestCase):
                    TestClasses.append(Class)

    return TestClasses

def test(os_type, xml=None):
    Suite = unittest.TestSuite()
    for Class in getTestClasses(os_type):
        SubSuite = unittest.TestLoader().loadTestsFromTestCase(Class)
        Suite.addTest(SubSuite)

    if xml is None:
        return unittest.TextTestRunner(verbosity=2).run(Suite)
    else:
        import xmlrunner
        with open(xml, 'wb') as f:
            return xmlrunner.XMLTestRunner(output=f).run(Suite)

def main():
    import argparse

    Parser = argparse.ArgumentParser(description='Test')
    Parser.add_argument("-s", "--os", type=str, dest="OS", required=True,
                        choices=["mac", "windows", "arch"],
                        help="Type of the target operating system.")
    Parser.add_argument("--xml", metavar="FILE", dest="Xml", default=None,
                        help="Write result to FILE as XML.")

    Args = Parser.parse_args()

    if test(utils.OSType(Args.OS), Args.Xml).wasSuccessful():
        return 0
    else:
        return 1

if __name__ == "__main__":
    sys.exit(main())
