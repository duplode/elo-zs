#!/bin/sh
pandoc folyami.md --filter pandoc-plot --mathjax --standalone -o folyami.html

