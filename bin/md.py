#!/usr/bin/env python3
# For Python 3

import sys, os
import argparse
import markdown as Md

HtmlHeaders = {"html5": '<!DOCTYPE html>\n<html lang="{lang}">'}

def parseMDFile(f, format="html5"):
    Html = Md.markdown(f.read(), output_format=format)
    return Html

def dressHtml(html, lang, title, css_url="", css_file="", format="html5"):
    Parts = [HtmlHeaders[format].format(lang=lang),
                      "<head>", "<title>{}</title>".format(title)]
    Parts.append('<meta charset="UTF-8">')
    if css_url:
        Parts.append('<link rel="stylesheet" href="{}">'.format(css_url))
    Parts += ["</head>", "<body>", html, "</body>", "</html>"]

    Html = '\n'.join(Parts)
    return Html

def main(argv):
    Parser = argparse.ArgumentParser()
    Parser.add_argument('MDFiles', metavar='FILE', type=str, nargs='+',
                        help='Markdown files to parse')
    Parser.add_argument("-s", "--style", metavar="URL", type=str, dest="StyleURL",
                        default="http://darksair.org/style/general.css",
                        help="The URL of the CSS")
    Parser.add_argument("-t", "--title", metavar="TEXT", type=str, dest="Title",
                        help="Title of the HTML")

    Args = Parser.parse_args(argv[1:])

    for MDFileName in Args.MDFiles:
        with open(MDFileName, 'r', encoding="utf-8") as MDFile:
            # The full path without extension
            Basename = os.path.splitext(MDFileName)[0]
            # The file name (w/o path) w/o extension
            Title = Args.Title
            if not Title:
                Title = os.path.basename(Basename)

            Html = parseMDFile(MDFile)
            FullHtml = dressHtml(Html, "en", Title, Args.StyleURL)

            with open(Basename + ".html", 'w', encoding="utf-8",
                      errors="xmlcharrefreplace") as HtmlFile:
                HtmlFile.write(FullHtml)
                HtmlFile.write('\n')

    return 0

if __name__ == "__main__":
    sys.exit(main(sys.argv))
