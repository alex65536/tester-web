#!/bin/bash
# Builds syntaxhighlighter

cd "../../third-party/syntaxhighlighter"

./node_modules/gulp/bin/gulp.js setup-project
./node_modules/gulp/bin/gulp.js build --brushes=cpp,delphi,plain --theme=swift

