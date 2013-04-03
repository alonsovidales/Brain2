#!/usr/bin/env python

import unittest, random, socket

__author__ = "Alonso Vidales"
__email__ = "alonso.vidales@tras2.es"
__date__ = "2013-04-03"

class ClientTest(unittest.TestCase):
    __host = "localhost"
    __fromPort = 2997
    __toPort = 2999
    __sockets = []
    __numOfVolatileKeys = 100
    __numOfStringKeys = 20

    def setUp(self):
        self.__sockets = []

        for port in xrange(self.__fromPort, self.__toPort + 1):
            brainSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            brainSocket.connect((self.__host, port))

            self.__sockets.append(brainSocket)

    def tearDown(self):
        for brainSocket in self.__sockets:
            brainSocket.close()

    def __exec(self, action, key, value = False):
        # Choose a random node from the list of available nodes, the queries should to
        # be consistents
        brainSocket = random.choice(self.__sockets)

        if value == False:
            brainSocket.sendall("%s %s a" % (action, key))
        else:
            brainSocket.sendall("%s %s %s a" % (action, key, value))

        return brainSocket.recv(1024)

    def test_setGetDelStrings(self):
        keys = {}

        # Insert all the string keys on the nodes
        for count in xrange(0, self.__numOfStringKeys):
            value = str(random.random())
            keys["set_%s" % (count)] = value
            self.__exec('set', "set_%s" % (count), value)

        # Get random keys and compare the values with the values on memory
        for count in xrange(0, 100):
            key = "set_%s" % (random.randrange(0, self.__numOfStringKeys, 2))
            value = self.__exec('get', key)

            self.assertEqual(value, keys[key])

        # Insert all the string keys on the nodes
        for count in xrange(0, self.__numOfStringKeys):
            value = str(random.random())
            keys["set_%s" % (count)] = value
            self.__exec('pset', "set_%s" % (count), value)

        # Get random keys and compare the values with the values on memory
        # Remove all the keys
        for count in xrange(0, self.__numOfStringKeys):
            self.__exec('del', "set_%s" % (count))

        # Remove and persist it
        for count in xrange(0, self.__numOfStringKeys):
            self.__exec('pdel', "set_%s" % (count))


    def test_setGetDelVolatile(self):
        keys = {}

        # Insert all the volatile keys on the nodes
        for count in xrange(0, self.__numOfVolatileKeys):
            value = str(random.random())
            keys["vol_%s" % (count)] = value
            self.__exec('vset', "vol_%s" % (count), value)

        # Get random keys and compare the values with the values on memory
        for count in xrange(0, 100):
            key = "vol_%s" % (random.randrange(0, self.__numOfVolatileKeys, 2))
            value = self.__exec('vget', key)

            self.assertEqual(value, keys[key])

        # Remove all the keys
        for count in xrange(0, self.__numOfVolatileKeys):
            self.__exec('vdel', "vol_%s" % (count))

if __name__ == '__main__':
    unittest.main()
