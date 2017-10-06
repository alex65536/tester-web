#!/bin/bash
# Cleans SyntaxHighlighter build files

cd "../../third-party/syntaxhighlighter/dist"

for FNAME in *
do
	[ "${FNAME}" != ".gitignore" ] && rm -f "${FNAME}"
done
