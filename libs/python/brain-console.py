#!/usr/bin/env python

import argparse
from brain import Brain

__author__ = "Alonso Vidales"
__email__ = "alonso.vidales@tras2.es"
__date__ = "2013-04-03"
__version__ = "0.1"

class BrainConsole:
    __actions = {
        # Strings
        'get': {
            'params': 0,
            'result': 'str',
            'desc': "get <key> : Get the string value of a key"
        },
        'set': {
            'params': 1,
            'result': 'str',
            'desc': "set <key> <value> : Set the value into a key"
        },
        'pset': {
            'params': 1,
            'result': 'str',
            'desc': "pset <key> <value> : Set the value into a key, and persist it into the warehouse"
        },
        'del': {
            'params': 0,
            'result': 'str',
            'desc': "del <key> : Removes a key and it contents"
        },
        'pdel': {
            'params': 0,
            'result': 'str',
            'desc': "pdel <key> : Removes a key and it contents from the node memory and warehouse"
        },

        # Volatile
        'vget': {
            'params': 0,
            'result': 'str',
            'desc': "get <key> : Set the value into a key. This kind of keys are not persisted into the warehouse."
        },
        'vset': {
            'params': 1,
            'result': 'str',
            'desc': "vset <key> <value> : Set the value into a key. This kind of keys are not persisted into the warehouse."
        },
        'vdel': {
            'params': 0,
            'result': 'str',
            'desc': "vdel <key> : Removes a key and it contents. This kind of keys are not persisted into the warehouse."
        },

        # Hash
        'hget': {
            'params': '*',
            'result': 'dict',
            'desc': "hget <key> [internal_key1, internal_key2, ...] : Return the values of all the specified internal keys. It not internal keys are specified, gets all the keys."
        },

        'hsetkv': {
            'params': 2,
            'result': 'str',
            'desc': "hsetkv <key> <internal_key> <internal_value> : Set the specified value to the given internal key of the hash"
        },

        'hpsetkv': {
            'params': 2,
            'result': 'str',
            'desc': "hpsetkv <key> <internal_key> <internal_value> : Set the specified value to the given internal key of the hash, and persists all the hash into the warehaouse after that"
        },

        'hdel': {
            'params': '*',
            'result': 'str',
            'desc': "hdel <key> [internal_key1, internal_key2, ...] : Removes the specified internal keys from the hash, or all the hash if not internal keys specified"
        },

        'hpdel': {
            'params': '*',
            'result': 'str',
            'desc': "hpdel <key> [internal_key1, internal_key2, ...] :\t\tRemoves the specified internal keys from the hash, on memory and warehouse, or all the hash if not internal keys specified"
        },

        'quit': {
            'params': '*',
            'result': 'str',
            'desc': "quit : Closes the current terminal"
        },
    }

    def __getHelp(self):
        for action, attribs in self.__actions.items():
            print attribs['desc']

    def run(self):
        try:
            self.__connection = Brain(
                self.__args.host,
                self.__args.port,
                verbose = False
            )
        except:
            print "Problem trying to connect to \"%s\" on port: %d" % (self.__args.host, self.__args.port)
            return False

        action = ''
        while action != 'quit':
            action = raw_input()
            if action != 'quit':
                if action == '?' or action == 'help':
                    self.__getHelp()
                else:
                    try:
                        [command, values] = action.split(' ', 1)

                        if command <> 'quit':
                            if command not in self.__actions:
                                print "Command not found: %s" % (action)
                            else:
                                try:
                                    if self.__actions[command]['params'] == '*':
                                        values = values.split(' ')
                                    else:
                                        values = values.split(' ', self.__actions[command]['params'])
                                    key = values.pop(0)

                                    if self.__actions[command]['params'] != '*' and len(values) < self.__actions[command]['params']:
                                        print "Command usage: %s" % (self.__actions[command]['desc'])
                                    else:
                                        if self.__actions[command]['params'] == '*':
                                            params = [key] + [values]
                                        else:
                                            params = [key] + values

                                        try:
                                            if command == 'del':
                                                command = 'remove'
                                            elif command == 'pdel':
                                                command = 'premove'

                                            result = getattr(self.__connection, command)(*params)
                                            if type(result) == type({}):
                                                for key, value in result.items():
                                                    print "%s: %s" % (key, value)
                                            else:
                                                print result
                                        except ValueError:
                                            print "Problem executing command"

                                except:
                                    print "Command usage: %s" % (self.__actions[action.strip()]['desc'])

                    except:
                        if action.strip() != '':
                            if action.strip() in self.__actions:
                                print "Command usage: %s" % (self.__actions[action.strip()]['desc'])
                            else:
                                print "Command not found: %s" % (action)

    def __init__(self):
        parser = argparse.ArgumentParser(
            description = "Brain console %s" % (__version__))

        parser.add_argument(
            '-ho',
            '--host',
            metavar = '<hostname>',
            default = 'localhost',
            help = 'Server hostname (default: localhost)')

        parser.add_argument(
            '--port',
            '-p',
            metavar = '<port>',
            default = 3697,
            type = int,
            help = 'Server port (default: 3697)')

        parser.add_argument(
            '--version',
            action='version',
            version="Brain Console: %s" % (__version__))

        parser.add_argument(
            '--verbose',
            '-v',
            help = 'Verbose mode')

        self.__args = parser.parse_args()

if __name__ == '__main__':
    BrainConsole().run()
