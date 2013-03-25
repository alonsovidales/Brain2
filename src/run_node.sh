#!/bin/bash
rm *.beam
erl -compile bootstrap
erl -noshell -s bootstrap start_node -name node$1 -verbose true
