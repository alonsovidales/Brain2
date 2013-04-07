#!/bin/bash
rm *.beam
echo "Compiling Vendor libs..."
erlc vendor/mochijson2.erl
echo "Compiling Warehouse manager..."
erlc warehouse_manager/ftp_manager.erl
echo "Compiling data types..."
erlc data_managers/data_hash.erl data_managers/data_volatile.erl data_managers/data_string.erl
echo "Compiling controllers..."
erlc controllers/ring_controller.erl controllers/data_controller.erl controllers/server_controller.erl controllers/manager_controller.erl
echo "Compiling libs..."
erlc includes/logging.erl includes/config_parser.erl
echo "Compiling bootstrap..."
erlc bootstrap.erl

erl -noshell -s bootstrap start_node -name node_1$1 -node_id node_1$1
