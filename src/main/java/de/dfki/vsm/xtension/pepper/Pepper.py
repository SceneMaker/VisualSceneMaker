import Queue
import argparse
import socket
import struct  # to check platform (32bit or 64bit)
import sys
import threading
from naoqi import ALProxy


# hack to import naoqi lib; TODO figure out how to add nicely to the path
# sys.path.insert(0, "C:\Users\Matthias\Uni\Hiwijob\DFKI\Pepper\Software\pynaoqi-python2.7-2.5.5.5-win32-vs2013\lib")


class SocketReader(threading.Thread):
    """
    Class to listen concurrently to an (already established) connection
    which can be written to from anywhere.

    Received messages are put into a Queue provided by the user
    upon creation.

    If the connection is terminated (detected by receiving None or catching
    a socket.error exception) the message "terminated" is placed in the queue
    and the thread is terminated.
    """
    def __init__(self, sock, msgQueue):
        threading.Thread.__init__(self, name="SocketReader")
        self.sock = sock
        self.msgQueue = msgQueue

    def run(self):
        try:
            while True:
                data = self.sock.recv(2048)
                if data is None:
                    self.msgQueue.put("terminated")
                    print "Connection was closed"
                    return
                self.msgQueue.put(data)
        except socket.error:
            self.msgQueue.put("terminated")
            print "Socket raised exception"


def validate_version():
    if not sys.version.startswith("2.7"):
        sys.exit("Error: This program needs Python version 2.7")


def validate_platform():
    if struct.calcsize("P") != 4:
        sys.exit("Error: This program needs a 32-bit version of Python")


if __name__ == '__main__':
    validate_version()
    validate_platform()


    parser = argparse.ArgumentParser(description="Program to enable communication between the Pepper plugin for "
                                                 "Visual Scene Maker and the Pepper robot.")
    parser.add_argument('-a', '--address', dest='host', default='localhost',
                        help='the IPv4 address of the machine running the VSM instance (default: localhost)')
    parser.add_argument('-p', '--port', dest='port', type=int, default=12345,
                        help='the port used for the VSM connection (default: 12345)')
    parser.add_argument('-ap', '--address-pepper', dest='host_pepper', default='pepper.local',
                        help='the IPv4 address of the Pepper robot (default: pepper.local)')
    parser.add_argument('-pp', '--port-pepper', dest='port_pepper', type=int, default=9559,
                        help='the port used for the Pepper connection (default: 9559)')
    args = parser.parse_args()


    host = args.host
    port = args.port

    host_pepper = args.host_pepper
    # host_pepper = "169.254.95.143"
    port_pepper = args.port_pepper

    # host = '127.0.0.1'  # localhost
    # port = 12345

    tts = ALProxy("ALTextToSpeech", host_pepper, port_pepper)

    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.connect((host, port))
        #s.setblocking(1)
        #print "Sending message..."
        s.send("Hello Java\n")
        #print "Waiting for answer..."
        msgQueue = Queue.Queue()    # Queue is synchronized
        socketReader = SocketReader(s, msgQueue)
        socketReader.start()
        while True:
            # answer = s.recv(1024)
            try:
                answer = msgQueue.get(block=True, timeout=2)
            except Queue.Empty:
                if socketReader.is_alive():
                    continue
                else:
                    print "Thread is dead, returning"
                    break
            if not answer:
                # if socket is in blocking mode (which it is by default), then
                # receiving nothing means the connection was terminated
                #print "empty"
                #continue
                break
            # s.send(answer)
            # print answer.rstrip()
            tts.say(answer)
            print "Python sends \"" + answer.rstrip() + "\" to Pepper at " + host_pepper + ":" + str(port_pepper)
    finally:
        s.close()

    #print answer


