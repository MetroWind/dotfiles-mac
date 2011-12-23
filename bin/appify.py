#!/usr/bin/env python3
# For Python 3.2+

import sys, os
import shutil

PLIST_HEAD = """<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
  <dict>
"""

PLIST_TAIL = """  </dict>
</plist>
"""

def buildDirStruct(parent_dir):
    """Build the app bundle dir structure under `parent_dir', and
    returns Contents dir, MacOS dir and Resources dir.
    """
    MacOSDir = os.path.join(parent_dir, "Contents/MacOS")
    ResDir = os.path.join(parent_dir, "Contents/Resources")
    os.makedirs(MacOSDir, 0o755, True)
    os.makedirs(ResDir, 0o755, True)
    return (os.path.join(parent_dir, "Contents"), MacOSDir, ResDir)

def makePlistKeyValue(key, value):
    """Generates and returns a key-value pair ready to be written into
    a plist file.  A newline is appended at the end."""
    Key = "    <key>{}</key>".format(key)
    Value = "    <string>{}</string>".format(value)
    return "\n".join([Key, Value, ""])

def makeId(app_name):
    return "com.darksair." + app_name.replace(' ', "")

def main():
    Usage = "%prog [[options] SCRIPT BUNDLE_NAME | --help]"
    Desc = "Wrap script SCRIPT (an UNIX executable) inside application bundle BUNDLE_NAME."

    import argparse
    
    OptParser = argparse.ArgumentParser(description=Desc)
    OptParser.add_argument("Script", metavar="SCRIPT", type=str, nargs=1,
                           help="The script file to wrap")
    OptParser.add_argument("Bundle", metavar="BUNDLE", type=str, nargs=1,
                           help="The name of bundle to wrap SCRIPT into")
    OptParser.add_argument("-i", "--icon", metavar="FILE", type=str,
                           dest="Icon",
                           help="Set the app icon to FILE")
    OptParser.add_argument("-n", "--name", metavar="NAME", type=str,
                           dest="Name",
                           help="Set the app name to NAME")

    Args = OptParser.parse_args()

    ScriptName = Args.Script[0]
    BundleName = Args.Bundle[0]
    AppName = os.path.basename(BundleName)

    if BundleName.endswith(".app"):
        AppName = AppName[:-4]
    else:
        BundleName += ".app"

    if Args.Name:
        AppName = Args.Name
    
    (ContentsDir, ExecDir, ResDir) = buildDirStruct(BundleName)

    # Copy script to bundle
    ExecName = os.path.join(ExecDir, os.path.basename(ScriptName))
    shutil.copyfile(ScriptName, ExecName)
    os.chmod(ExecName, 0o755)

    PlistFile = open(os.path.join(ContentsDir, "Info.plist"), 'w')
    PlistFile.write(PLIST_HEAD)

    # Specify executable
    PlistFile.write(makePlistKeyValue(
        "CFBundleExecutable", os.path.basename(ExecName)))
    # Specify id
    PlistFile.write(makePlistKeyValue(
        "CFBundleIdentifier", makeId(AppName)))
    # Specify version
    PlistFile.write(makePlistKeyValue(
        "CFBundleShortVersionString", "3.14"))
    # Write icon info
    if Args.Icon:
        shutil.copy(Args.Icon, ResDir)
        PlistFile.write(makePlistKeyValue(
            "CFBundleIconFile", os.path.basename(Args.Icon)))
    # Write name info
    PlistFile.write(makePlistKeyValue(
        "CFBundleDisplayName", AppName))
    PlistFile.write(makePlistKeyValue(
        "CFBundleName", AppName))

    # Finish up Info.plist
    PlistFile.write(PLIST_TAIL)
    PlistFile.close()

    return 0

if __name__ == "__main__":
    sys.exit(main())
