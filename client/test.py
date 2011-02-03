#!/usr/bin/python
import socket

HOST = 'localhost'    # The remote host
PORT = 9234              # The same port as used by the server
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect((HOST, PORT))
s.send('{"login": "user1", "password": "pwd"}\0')
s.send('{"type": "RequestBuy", "data": {"price": "10", "tool": "tool1"}}\0')
data = s.recv(1024)
print 'Received', repr(data)

s.send('{"type": "GetRequests", "data": null}\0')
data = s.recv(1024)
print 'Received', repr(data)

raw_input("Press Enter to continue...")
s.close()
