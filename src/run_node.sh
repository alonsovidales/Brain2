#!/bin/bash
rm *.beam
erlc includes/logging.erl includes/config_parser.erl bootstrap.erl
erl -noshell -s bootstrap start_node -name node$1 -verbose true
