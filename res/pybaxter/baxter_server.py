#!/usr/bin/env python
# Echo client program
#Return errors always must end wit a new line character "\n"
#Errors:
#1: OK
#2: No method

import socket
import json
import head_gestures
BUFFER_SIZE = 1024  # Normally 1024, but we want fast response

class CommunicationSocket(object):
    def __init__(self):
        self.TCP_IP = '192.168.0.10'
        self.TCP_PORT = 1313
        self.start_listen()

    def start_listen(self):
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.bind((self.TCP_IP, self.TCP_PORT))
        print "Listening..."
        s.listen(1)

        self.conn, addr = s.accept()
        print 'Connection address:', addr
        while 1:
            data = self.conn.recv(BUFFER_SIZE)
            if not data: continue
            print "received data:", data
            json_object = json.loads(data)
            method = json_object['method']
            params = json_object['params']
            gestures = head_gestures.Gestures()
            methods = [method_iter for method_iter in dir(gestures) if callable(getattr(gestures, method_iter))]
            print"aqui", method
            if method in methods:
                try:
                    func = getattr(gestures, method)
                    if len(params) > 0 and params != None and params != "null":
                        func(params)
                    else:
                        func()
                    self.conn.send("1\n")  # To indicate everything went ok
                except AttributeError:
                    self.conn.send("0\n")  # To indicate error
                    print "dostuff not found"
            else:
                print "not found",data
            self.conn.send("2\n")  # To indicate everything went ok

        self.conn.close()


if __name__ == '__main__':
    communication = CommunicationSocket()
    print "Hola"


