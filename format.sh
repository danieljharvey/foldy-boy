#!/bin/bash

echo "Formatting..."

find "./src" -type f -iname *.purs -exec purty --write '{}' \;

find "./test" -type f -iname *.purs -exec purty --write '{}' \;

echo "Done!"
