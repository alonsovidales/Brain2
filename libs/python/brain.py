#!/usr/bin/env python

import socket

__author__ = "Alonso Vidales"
__email__ = "alonso.vidales@tras2.es"
__date__ = "2013-04-06"

class Brain:
    def __exec(self, action, key, value = False):
        if self.__verbose:
            print "Exec: %s %s %s" % (action, key, value)

        if value == False:
            self.__socket.sendall("%s %s" % (action, key))
        else:
            self.__socket.sendall("%s %s %s" % (action, key, value))

        result = self.__socket.recv(4096)

        if result == "null":
            return None

        if result[0] == '-':
            return result[1:]

        if result[0:5] == "error":
            raise Exception("[Brain] Error trying to execute the query: %s" % (result))

        return result

    """
    Stirng data type queries
    """
    def get(self, inKey):
        return self.__exec("get", inKey)

    def set(self, inKey, inValue):
        return self.__exec("set", inKey, inValue)

    def pset(self, inKey, inValue):
        return self.__exec("pset", inKey, inValue)

    def remove(self, inKey):
        return self.__exec("del", inKey)

    def premove(self, inKey):
        return self.__exec("pdel", inKey)

    """
    Volatile data type queries
    """
    def vget(self, inKey):
        return self.__exec("vget", inKey)

    def vset(self, inKey, inValue):
        return self.__exec("vset", inKey, inValue)

    def vdel(self, inKey):
        return self.__exec("vdel", inKey)

    """
    Hash data type queries
    """
    def __parseHashResultAsDict(self, inResult):
        return inResult

    def hget(self, inKey, inIntKeys = []):
        if len(inIntKeys) > 0:
            return self.__parseHashResultAsDict(self.__exec("hget", inKey, "%s" % (" ".join(inIntKeys))))

        return self.__parseHashResultAsDict(self.__exec("hget", inKey))

    def hsetkv(self, inKey, inInKey, inValue):
        return self.__exec("hsetkv", inKey, "%s %s" % (inInKey, inValue))

    def hpsetkv(self, inKey, inInKey, inValue):
        return self.__exec("hpsetkv", inKey, "%s %s" % (inInKey, inValue))

    def hdel(self, inKey, inIntKeys = []):
        if len(inIntKeys) > 0:
            return self.__parseHashResultAsDict(self.__exec("hdel", inKey, "%s" % (" ".join(inIntKeys))))

        return self.__parseHashResultAsDict(self.__exec("hdel", inKey))

    def hpdel(self, inKey, inIntKeys = []):
        if len(inIntKeys) > 0:
            return self.__parseHashResultAsDict(self.__exec("hpdel", inKey, "%s" % (" ".join(inIntKeys))))

        return self.__parseHashResultAsDict(self.__exec("hpdel", inKey))

    def close(self):
        self.__socket.close()

    def __del__(self):
        self.close()

    def __init__(self, host = 'localhost', port = 3697, verbose = False):
        self.__verbose = verbose
        self.__socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.__socket.connect((host, int(port)))
