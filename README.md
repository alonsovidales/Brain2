Brain is an on memory active cache system, distributed, and highly parallelizable. Brain is able to read from inexpensive storage systems like Amazon S3, or an FTP, keep in memory the information, and after a specified period of inactivity dump the data to the storage system again.
Brain stores the data on the memory of the node who made the request it, but another node can access to the data in a transparent way for the client, doesn't matters if the data is on the memory of the local node, or in any other node of the system, is a distributed-memory system.

## Overview and deployment specifications

The classical architecture for a high performance and high availability system without Brain use to be:

<p align="center">
    <img src="https://raw.github.com/alonsovidales/Brain2/master/doc/imgs/classic_deployment_schema.png" height="350" />
</p>

We have the redundant database servers + cache servers + front servers. Keep all the information on the database servers indexed and ready to be accessed if too expensive, and needs an important waste of resources on maintance, redudance, etc. The action of scale the database is a very heavy and complex task.

Brain is designed for applications like video games, e-commerce, etc where the system do a heavy usage of some records on a specific period of time (user information, etc), and after this period, the records keeps inactive for a long period of time, then is not necessary to store them on memory, or keep them on a hight performance storage system (Databases, etc), we can store them on a cheap storage system.

For example, on the common usage for online games, when a user starts to play, the system loads all the information. Along the period of time who the user are playing, the system need to read / update a lot of times this records, and after finish to play keep the user records on memory is not necessary until the user plays again:

<p align="center">
    <img src="https://raw.github.com/alonsovidales/Brain2/master/doc/imgs/requests_time.png" height="100" />
</p>

Now, let's check the CPU / Memory usage on the front servers for the classic schema:

<p align="center">
    <img src="https://raw.github.com/alonsovidales/Brain2/master/doc/imgs/front_servers_memory_cpu.png" height="90" />
</p>

The front servers uses a lot of CPU in order to process all the information and create the output for the clients. The memory is underused.

Brain is designed to work keeping the information of the active users on the memory of the front servers, obtaining a better usage of the resources:

<p align="center">
    <img src="https://raw.github.com/alonsovidales/Brain2/master/doc/imgs/front_servers_memory_cpu_with_brain.png" height="90" />
</p>

The usage of the memory is increased, keeping the information of the active users for this node, and the CPU usage is less than on a classic schema, this is because of we don't need to use the network, create complex SQL calls to get the data, etc. The most important part of the queries are local queries in order to obtain or modify information located on the node local memory.

With Brain the arquitecture necessary for a hight performance, and hight availability system is the next:

<p align="center">
    <img src="https://raw.github.com/alonsovidales/Brain2/master/doc/imgs/brain_deployment.png" height="230" />
</p>

We can replace the database servers by an inexpensive storage system like Amazon S3 or a FTP system, and the cache layer will be located on the front servers.

Is important to note that a "Sticky" load balancer is the best choice in order to obtain a better performance, the information of the user will be stored on the local memory of the front server assigned to the user.

One of the most important features of this architecture, is that the cost of this architecture is directly related to the number of active users, and can scale really fast, on a classic schema the costs are related to the number of data stored.

## Building and Installing Brain

### Prerequisites

Brain is designed for Linux OS. Before compile or install Brain, is necessary to have installed before:
 - Erlang : V5.8.5 or better
 - Python : V2.7.3 or better

### Installation

In order to install Brain, just execute "make" or "make install" as super user. This will compile all the erlang code, and create the beam files into the /usr/local/brain/ directory.

The executable files will be located into the /usr/local/bin/ directory:
 - brain-console.py : Used to connect and interact with one of the nodes
 - brain-manager : Launch the manager system in backplain
 - brain-server : Launch the local server node

The installation will install the [Brain Python libraries](https://github.com/alonsovidales/Brain2/tree/master/libs/python/brain).

Check the configuration section after install the system

In order to allow the communication between nodes, the value of the parameter "secret" on the config file should to be the same on all the nodes and managers.

Check: http://www.erlang.org/doc/getting_started/conc_prog.html#id67450

### Configuration

Brain searchs for the config file at the next locations:
 - /etc/brain_node.conf
 - /etc/brain/brain_node.conf
 - etc/brain_node.conf -> Search on the current path where the system is executed.

In order to configure the system, just copy the etc/brain_node.conf to the /etc/ system directory, and edit it, the file includes comments about what the parameters do.

## The managers

The system needs some manager instances in order to control all the available node instances on the ring, check the health of each one, and keep updated the list of available instances on all the nodes.

At least one "manager" instance is necessary in order to use the distributed memory strategy, you can specify one or more on the config variable "manager_servers" using a coma separated list.

If one of the manager servers fails, the rest of servers can still control the ring, is a redundant system without primary or slave servers.

## Brain console

Brain includes a console that help you to interact with the servers work with the data, check the status of the node shutdown it, etc.

By default the console will try to connect to the local node on the port 3697, if you want to see the different configuration parameters use:

brain-console.py --help
  
After open the console, use the "?" character to see all the available options:

<p align="center">
    <img src="https://raw.github.com/alonsovidales/Brain2/master/doc/imgs/brain_console_help.png" />
</p>
