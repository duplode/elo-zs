#!/bin/sh
pandoc folyami.md --filter pandoc-plot --pdf-engine lualatex -o folyami.pdf

