# Brain
Brain is an on memory active cache system, distributed, and hightly pararelizabled. Brain is able to read from unexpensive storage systems like Amazon S3, or an FTP, keep in memory the information, and after a specified period of inactivity dump the datas to the storage system again.
Brain stores the datas on the memory of the node who has request it, but another node can access to the data in a transparent way for the client, doesn't matters if the data is on the memory of the local node, or in any other node of the system.

## What is Brain, and for whom ussage is Brain recomended

The classical arquitecture for a hight performance, and high availability system is:

<img src="https://raw.github.com/alonsovidales/Brain2/master/doc/imgs/classic_deployment_schema.png" />
![ScreenShot](https://raw.github.com/alonsovidales/Brain2/master/doc/imgs/classic_deployment_schema.png)

We have the redundant database servers + cache servers + front servers, keep all the information on the database servers idexed and ready to be accessed if too expensive, and needs an important waste of resources on maintance, redudance, etc, use a cache is expensive too. Scale the database is a very heavy task.

Brain is designed for some applications like video games, e-commerce, etc where the requests for a user along the time looks like:

![ScreenShot](https://raw.github.com/alonsovidales/Brain2/master/doc/imgs/requests_time.png)

The users use to keep a lot of time inactive but when they are using the application, they do a lot of requests to the database in a short period of time to read and moify the information stored. For example in a vide game, the users use to play some hours, and keep inactive the rest of the day, this is the perfet application for Brain.

Now, let's check the CPU / Memory ussage on the front servers for a classic schema:

![ScreenShot](https://raw.github.com/alonsovidales/Brain2/master/doc/imgs/front_servers_memory_cpu.png)

The front servers uses a lot of CPU in order to process all the information and create the output for the clients be the memory is underussed.

Brain is designed to work keeping the information of the active users on the memory of the front servers, obtaining a better ussage of the resources:

![ScreenShot](https://raw.github.com/alonsovidales/Brain2/master/doc/imgs/front_servers_memory_cpu_with_brain.png)

The ussage of the memory is increased, keeping the information of the active users for this node, and the CPU usage have a reduction, this is because of we don't need to use the network, create complex SQL calls to get the datas, etc, the most important par of the queries are local queries.

With Brain the arquitecture necessary for a hight performance, and hight availability system is the next:
![ScreenShot](https://raw.github.com/alonsovidales/Brain2/master/doc/imgs/brain_deployment.png)


