#!/usr/bin/env python

import unittest, random
from brain import Brain

__author__ = "Alonso Vidales"
__email__ = "alonso.vidales@tras2.es"
__date__ = "2013-04-03"

class ClientTest(unittest.TestCase):
    __host = "localhost"
    __fromPort = 5997
    __toPort = 5999
    __connections = []
    # Hash test config
    __numOfHashPrimaryKeys = 10
    __numOfHashIntKeys = 5
    # Volatile test config
    __numOfVolatileKeys = 10000
    # String test config
    __numOfStringKeys = 20

    def setUp(self):
        self.__connections = []

        for port in xrange(self.__fromPort, self.__toPort + 1):
            self.__connections.append(Brain(port = port))

    def tearDown(self):
        for brainSocket in self.__connections:
            brainSocket.close()

    def __getConnection(self):
        # Choose a random node from the list of available nodes, the queries should to
        # be consistents
        return random.choice(self.__connections)

    def test_setGetDelStrings(self):
        keys = {}

        # Insert all the string keys on the nodes
        for count in xrange(0, self.__numOfStringKeys):
            value = str(random.random())
            keys["set_%s" % (count)] = value
            self.__getConnection().set("set_%s" % (count), value)

        # Get random keys and compare the values with the values on memory
        for count in xrange(0, 100):
            key = "set_%s" % (random.randrange(0, self.__numOfStringKeys, 2))
            value = self.__getConnection().get(key)

            self.assertEqual(value, keys[key])

        # Insert all the string keys on the nodes
        for count in xrange(0, self.__numOfStringKeys):
            value = str(random.random())
            keys["set_%s" % (count)] = value
            self.__getConnection().pset("set_%s" % (count), value)

        # Get random keys and compare the values with the values on memory
        # Remove all the keys
        for count in xrange(0, self.__numOfStringKeys):
            self.__getConnection().remove("set_%s" % (count))

        # Remove and persist it
        for count in xrange(0, self.__numOfStringKeys):
            self.__getConnection().premove("set_%s" % (count))

        # Try to get a string not previously setted
        self.assertFalse(self.__getConnection().get('aaaa'))


    def test_setGetDelVolatile(self):
        keys = {}

        # Insert all the volatile keys on the nodes
        for count in xrange(0, self.__numOfVolatileKeys):
            value = str(random.random())
            keys["vol_%s" % (count)] = value
            self.__getConnection().vset("vol_%s" % (count), value)

        # Get random keys and compare the values with the values on memory
        for count in xrange(0, 100):
            key = "vol_%s" % (random.randrange(0, self.__numOfVolatileKeys, 2))
            value = self.__getConnection().vget(key)

            self.assertEqual(value, keys[key])

        # Remove all the keys
        for count in xrange(0, self.__numOfVolatileKeys):
            self.__getConnection().vdel("vol_%s" % (count))

        # Try to get a volatile not previously setted
        self.assertFalse(self.__getConnection().vget('aaaa'))

    def test_setGetDelHash(self):
        keys = {}

        # Insert all the volatile keys on the nodes
        for primKey in xrange(0, self.__numOfHashPrimaryKeys):
            keys["hash_%s" % (primKey)] = {}
            for intKey in xrange(0, self.__numOfHashIntKeys):
                value = str(random.random())
                keys["hash_%s" % (primKey)]["int_%s" % (intKey)] = value
                self.__getConnection().hsetkv("hash_%s" % (primKey), "int_%s" % (intKey), value)

        # Check the get of all the values one by one
        for primKey in xrange(0, self.__numOfHashPrimaryKeys):
            for intKey in xrange(0, self.__numOfHashIntKeys):
                value = self.__getConnection().hget("hash_%s" % (primKey), ["int_%s" % (intKey)])
                self.assertEqual({"int_%s" % (intKey): keys["hash_%s" % (primKey)]["int_%s" % (intKey)]}, value)

        # Remove all the keys
        for count in xrange(0, self.__numOfVolatileKeys):
            self.__getConnection().hdel("hash_%s" % (count))

if __name__ == '__main__':
    unittest.main()
