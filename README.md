Brain is an on memory active cache system, distributed, and highly parallelizable. Brain is able to read from unexpensive storage systems like Amazon S3, or an FTP, keep in memory the information, and after a specified period of inactivity dump the data to the storage system again.
Brain stores the data on the memory of the node who made the request it, but another node can access to the data in a transparent way for the client, doesn't matters if the data is on the memory of the local node, or in any other node of the system, is a distributed-memory system.

## Deployment specifications

The classical architecture for a high performance and high availability system use to be:

<p align="center">
    <img src="https://raw.github.com/alonsovidales/Brain2/master/doc/imgs/classic_deployment_schema.png" height="350" />
</p>

We have the redundant database servers + cache servers + front servers. Keep all the information on the database servers indexed and ready to be accessed if too expensive, and needs an important waste of resources on maintance, redudance, etc. The action of scale the database is a very heavy and complex task.

Brain is designed for applications like video games, e-commerce, etc where the system do a heavy usage of some records on a specific period of time (user information, etc), and after this period, the records keeps inactive for a long period of time, then is not necessary to store them on memory, or keep them on a hight performance storage system (Databases, etc), we can store them on a cheap storage system.

For example, for a video game, a user access to the game, the system loads all the information, and for the period of time who the user are playing, the system need to read / update a lot of times this records, but when the player is not playing, the datas can be stored:

<p align="center">
    <img src="https://raw.github.com/alonsovidales/Brain2/master/doc/imgs/requests_time.png" height="100" />
</p>

The users use to keep a lot of time inactive but when they are using the application, they do a lot of requests to the database in a short period of time to read and moify the information stored. For example in a vide game, the users use to play some hours, and keep inactive the rest of the day, this is the perfet application for Brain.

Now, let's check the CPU / Memory ussage on the front servers for a classic schema:

<p align="center">
    <img src="https://raw.github.com/alonsovidales/Brain2/master/doc/imgs/front_servers_memory_cpu.png" height="90" />
</p>

The front servers uses a lot of CPU in order to process all the information and create the output for the clients be the memory is underussed.

Brain is designed to work keeping the information of the active users on the memory of the front servers, obtaining a better ussage of the resources:

<p align="center">
    <img src="https://raw.github.com/alonsovidales/Brain2/master/doc/imgs/front_servers_memory_cpu_with_brain.png" height="90" />
</p>

The ussage of the memory is increased, keeping the information of the active users for this node, and the CPU usage have a reduction, this is because of we don't need to use the network, create complex SQL calls to get the data, etc, the most important par of the queries are local queries.

With Brain the arquitecture necessary for a hight performance, and hight availability system is the next:

<p align="center">
    <img src="https://raw.github.com/alonsovidales/Brain2/master/doc/imgs/brain_deployment.png" height="230" />
</p>
