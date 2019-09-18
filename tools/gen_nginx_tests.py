#!/usr/bin/env python

import json
import os


URL = "https://github.com/http2jp/hpack-test-case"
PATH = "test/hpack-test-case"

def main():
    os.system("git clone --depth 1 --single-branch " + URL + " " + PATH)
    path = os.path.join(PATH, "nghttp2-change-table-size")

    print "%% Test data generated from:"
    print "%%%%     %s"  % URL
    print "\n"

    for path, dnames, fnames in os.walk(path):
        for fname in sorted(fnames):
            if not fname.endswith(".json"):
                continue
            fpath = os.path.join(path, fname)
            with open(fpath) as handle:
                data = json.load(handle)

            cases = []

            for case in data["cases"]:
                c = []
                if "header_table_size" in case:
                    c.append("%d" % case["header_table_size"])
                else:
                    c.append("undefined")
                c.append("<<\"%s\">>" % case["wire"])
                headers = []
                for header in case["headers"]:
                    assert len(header) == 1
                    k = header.keys()[0].encode("utf-8").encode("string_escape")
                    v = header[k].encode("utf-8").encode("string_escape")
                    v = v.replace('"', '\\"')
                    headers.append("{<<\"%s\">>, <<\"%s\">>}"% (k, v))
                c.append("[\n%s\n]" % ",\n".join(headers))
                cases.append("{\n%s\n}" % ",\n".join(c))

            print "{%s, [" % fname[:-5]
            print ",\n".join(cases)
            print "]}."

if __name__ == "__main__":
    main()