import socket, json

__author__ = "Alonso Vidales"
__email__ = "alonso.vidales@tras2.es"
__date__ = "2013-04-06"

class Brain:
    def __exec(self, action, key = False, value = False):
        if self.__verbose:
            print "Exec: %s %s %s" % (action, key, value)

        if key == False:
            self.__socket.sendall("%s" % (action))
        elif value == False:
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
        """
        Return the value contained in the given key for a string data type as a string
        or None if the key was not found
        inKey -- The string with the key name
        """
        return self.__exec("get", inKey)

    def set(self, inKey, inValue):
        """
        Set a value for a key of a string data type
        inKey -- The string with the key name
        inValue -- The value to be setted to the key
        """
        return self.__exec("set", inKey, inValue)

    def pset(self, inKey, inValue):
        """
        Set a value for a key or a string data type and persist it inmediatly into the
        permanent storage system
        inKey -- The string with the key name
        inValue -- The value to be setted to the key
        """
        return self.__exec("pset", inKey, inValue)

    def remove(self, inKey):
        """
        Removes a key and its value for a string data type
        inKey -- The string with the key name
        """
        return self.__exec("del", inKey)

    def premove(self, inKey):
        """
        Removes a key and its value for a string data type, and removes the key from the
        permanent storage system inmediatly
        inKey -- The string with the key name
        """
        return self.__exec("pdel", inKey)

    """
    Volatile data type queries
    """
    def vget(self, inKey):
        """
        Return the value contained in the given key for a volatile data type as a string
        or None if the key was not found
        inKey -- The string with the key name
        """
        return self.__exec("vget", inKey)

    def vset(self, inKey, inValue):
        """
        Set a value for a key of a volatile data type
        inKey -- The string with the key name
        inValue -- The value to be setted to the key
        """
        return self.__exec("vset", inKey, inValue)

    def vdel(self, inKey):
        """
        Removes a key and its value for a volatile data type
        inKey -- The string with the key name
        """
        return self.__exec("vdel", inKey)

    """
    Hash data type queries
    """
    def hget(self, inKey, inIntKeys = []):
        """
        Return the value of all or a set of keys on a hash data type value as a dictionary
        Returns None if the primary key doesn't exists
        inKey -- The primary key as string of the hash
        inIntKeys -- The internal keys of the hash to be returned, thi parameter is optional,
                     if is not specified returns all the keys
        """
        if len(inIntKeys) > 0:
            result = self.__exec("hget", inKey, "%s" % (" ".join(inIntKeys)))
            if result != None and result[0] == '{':
                return json.loads(result)

            return result

        result = self.__exec("hget", inKey)
        if result != None and result[0] == '{':
            return json.loads(result)

        return result

    def hsetkv(self, inKey, inInKey, inValue):
        """
        Set the specified value to the specified internal key of the hash associated to the given
        primary key
        inKey -- The primary key of the hash
        inInKey -- The internal key to be setted
        inValue -- The value to be setted to the key
        """
        return self.__exec("hsetkv", inKey, "%s %s" % (inInKey, inValue))

    def hpsetkv(self, inKey, inInKey, inValue):
        """
        Set the specified value to the specified internal key of the hash associated to the given
        primary key, and persists the complete hash on the permanent storage system immediately
        inKey -- The primary key of the hash
        inInKey -- The internal key to be setted
        inValue -- The value to be setted to the key
        """
        return self.__exec("hpsetkv", inKey, "%s %s" % (inInKey, inValue))

    def hdel(self, inKey, inIntKeys = []):
        """
        Removes the specificed internal keys. If no internal key is specified, removes the complete hash
        inKey -- The primary key of the hash
        inIntKeys -- List of internal keys to be removed, this param is optional, if not specified, remove
                     all the hash
        """
        if len(inIntKeys) > 0:
            return self.__exec("hdel", inKey, "%s" % (" ".join(inIntKeys)))

        return self.__exec("hdel", inKey)

    def hpdel(self, inKey, inIntKeys = []):
        """
        Removes the specificed internal keys. If no internal key is specified, removes the complete hash
        After execute the action, persists the hash on the permanent storage system, or removes it if the
        hash is empty
        inKey -- The primary key of the hash
        inIntKeys -- List of internal keys to be removed, this param is optional, if not specified, remove
                     all the hash
        """
        if len(inIntKeys) > 0:
            return self.__exec("hpdel", inKey, "%s" % (" ".join(inIntKeys)))

        return self.__exec("hpdel", inKey)

    """
    Special queries to control some servers features
    """
    def shutdown(self):
        """
        Safety shutdown of the local node
        """
        return self.__exec("shutdown")

    def info(self):
        """
        Returns a list of information about the local node, number of queries per second, memory ussage, etc.
        """
        return self.__exec("info").split('|')

    def close(self):
        """
        Closes the connection with the node
        """
        self.__socket.close()

    def __del__(self):
        self.close()

    def __init__(self, host = 'localhost', port = 3697, verbose = False):
        """
        Open the connection with the node
        host -- A string with the node name (default: 'localhost')
        port -- The port to be used (default: 3697)
        verbose -- Verbose mode, if true prints information about queries, etc (default: False)
        """
        self.__verbose = verbose
        self.__socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.__socket.connect((host, int(port)))
