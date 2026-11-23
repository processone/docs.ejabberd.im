#!/usr/bin/env python3
"""Fail when the generated docs PDF is truncated.

WeasyPrint can stop laying out pages without raising an error, which leaves a
short PDF whose table of contents points to page 0 for every missing chapter.
"""
import re
import sys

from pypdf import PdfReader

MIN_PAGES = 400      # 26.04 had 516 pages; the truncated 26.07/26.09 have 157
LAST_CHAPTER = "Roadmap"

path = sys.argv[1]
reader = PdfReader(path)
pages = len(reader.pages)
errors = []

if pages < MIN_PAGES:
    errors.append(f"only {pages} pages, expected at least {MIN_PAGES}")

# A TOC entry whose target is missing renders its page number as 0;
# pypdf extracts it as "0Clustering" (pdftotext as "Clustering   0").
toc = "\n".join(reader.pages[i].extract_text() or "" for i in range(min(15, pages)))
zero = [l.strip() for l in toc.splitlines() if re.search(r"^0[^\d.,]|\S\s+0$", l.strip())]
if zero:
    errors.append(f"{len(zero)} TOC entries point to page 0, first: {zero[:5]}")

outline = [o.title for o in reader.outline if not isinstance(o, list)]
if not outline or outline[-1] != LAST_CHAPTER:
    errors.append(f"last PDF bookmark is {outline[-1:]!r}, expected {LAST_CHAPTER!r}")

if errors:
    print(f"{path}: PDF looks truncated:", *errors, sep="\n  ")
    sys.exit(1)
print(f"{path}: OK, {pages} pages, ends with {LAST_CHAPTER!r}")
