INSTALL_DIR=/usr/local/brain/
INSTALL_BIN=/usr/local/bin/

install:
	@mkdir -p $(INSTALL_DIR)
	@echo "Compiling Vendor libs..."
	@erlc -o $(INSTALL_DIR) vendor/mochijson2.erl
	@echo "Compiling Warehouse manager..."
	@erlc -o $(INSTALL_DIR) warehouse_manager/ftp_manager.erl
	@echo "Compiling data types..."
	@erlc -o $(INSTALL_DIR) data_managers/data_hash.erl data_managers/data_volatile.erl data_managers/data_string.erl
	@echo "Compiling controllers..."
	@erlc -o $(INSTALL_DIR) controllers/ring_controller.erl controllers/data_controller.erl controllers/server_controller.erl controllers/manager_controller.erl
	@echo "Compiling libs..."
	@erlc -o $(INSTALL_DIR) includes/logging.erl includes/config_parser.erl
	@echo "Compiling bootstrap..."
	@erlc -o $(INSTALL_DIR) bootstrap.erl

	@mkdir -p $(INSTALL_DIR)etc/
	@cp etc/brain_node.conf $(INSTALL_DIR)etc/

        # Install manager
	@cp brain-manager $(INSTALL_BIN)
	@chmod a+x $(INSTALL_BIN)brain-manager

        # Install server
	@cp brain-server $(INSTALL_BIN)
	@chmod a+x $(INSTALL_BIN)brain-server

	@cd ../libs/python/; python setup.py install

	@echo "*************************************************************************************************"
	@echo " Brain system installation success"
	@echo " To configure the system copy the file on $(INSTALL_DIR)etc/ to the /etc/ directory, and edit it"
	@echo " Use brain-manager to launch a manager instance"
	@echo " Use brain-server to launch a brain node instance"
	@echo "*************************************************************************************************"

all: install

clean:
	@rm $(INSTALL_BIN)brain-manager
	@rm $(INSTALL_BIN)brain-server
	@rm $(INSTALL_BIN)brain-console
	@rm -rf $(INSTALL_DIR)
	@pip uninstall brain
	@echo "Brain and all the installed files has been removed"
