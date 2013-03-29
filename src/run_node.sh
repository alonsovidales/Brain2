#!/bin/bash
rm *.beam
erlc data_managers/data_hash.erl data_managers/data_volatile.erl data_managers/data_string.erl
erlc controllers/data_controller.erl
erlc includes/logging.erl includes/config_parser.erl
erlc bootstrap.erl
erl -noshell -s bootstrap start_node -name node$1 -node_id node$1 -verbose true
