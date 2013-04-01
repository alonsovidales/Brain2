#!/bin/bash
rm *.beam
echo "Compiling data types..."
erlc data_managers/data_hash.erl data_managers/data_volatile.erl data_managers/data_string.erl
echo "Compiling controllers..."
erlc controllers/ring_controller.erl controllers/data_controller.erl controllers/server_controller.erl controllers/manager_controller.erl
echo "Compiling libs..."
erlc includes/logging.erl includes/config_parser.erl
echo "Compiling bootstrap..."
erlc bootstrap.erl

echo "Strating manager 1..."
erl -noshell -s bootstrap start_manager -name manager@localhost -setcookie 123 -verbose true 
#sleep 2

#echo "Strating node 1..."
#erl -noshell -s bootstrap start_node -name node_1$1 -node_id node_1$1 -port 8888 -verbose true &

#echo "Strating node 2..."
#erl -noshell -s bootstrap start_node -name node_2$1 -node_id node_2$1 -port 8889 -verbose true
