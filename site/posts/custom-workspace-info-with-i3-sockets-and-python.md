---
title: Custom Workspace Info with i3, Sockets, and Python
published: 2015-11-29
teaser: i3 is an extremely flexible window manager, and its Inter-Process Communication facility makes it extensible as well. Here I'll show how to harness that extensibility with Python.	custom-workspace-info-with-i3-sockets-and-python.
tags: [ricing, i3, linux, python]
---

I recently switched from [BSPWM](https://github.com/baskerville/bspwm) to [i3](https://i3wm.org/), and I'm very impressed. It has nearly every feature I've wished for from the WMs I've used in the past. The only thing I missed was the [Lemonbar](https://github.com/LemonBoy/bar) setup I perfected with BSPWM. i3 has its own sysinfo bar, and won't send info to another one by default.

Thankfully, i3 provides an interprocess communication interface. External programs or scripts can connect to an i3 instance through a socket in order to send commands, make queries, or subscribe to various types of events. While there are a number of existing python libraries to facilitate communication with i3, I found none that were both maintained and bug-free. Given the simplicity of these libraries, I decided to write what I wanted from scratch rather than troubleshoot someone else's code.

Having learned the rudiments of communicating with i3 using Python, I thought I'd explain the more technical points for others in a similar situation. I'll show how to connect to i3, subscribe to workspace events, and output a string based on the current workspace info. This was my use-case, but the same methods can be employed to a variety of other ends.

# Necessary Modules

```python
import socket
import struct
import json
```

# Initializing a Socket

Firstly, if you're going to be dealing with more than one socket (and if you want to subscribe to events, you will), you're going to want to create a Socket class. We'll initialize sockets in the init method of that class. The steps for initialization are straightforward: create a socket object with the socket module, set a timeout, and connect.

```Python
class Socket():
    def __init__(self):
        #must define AF_UNIX or unsupported, SOCK_STREAM is socket type
        self.sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.sock.settimeout(20)
        self.sock.connect(socket_path)
```

All the functions that follow will be methods of the above Socket class.

# Formatting Message

All messages sent to i3 are composed of the following components:
1. The "magic string" of "i3-ipc", which lets i3 know when a message begins.
2. The length of the payload, as an unsigned integer.
3. The type of message being sent, as an unsigned integer. See the i3-ipc documentation for a full list of message types and their identifiers.
4. The payload, that is to say the message sent to i3, if any.
The message itself is sent as an 8 bit string.

The following method will assemble a message to the above specifications, given a message type and payload string.

```python
def format_msg(self, msg_type, payload):
    payload_length = len(payload.encode('utf-8'))
    msg_length = struct.pack('I', msg_length).decode('utf-8')
    msg_type = struct.pack('I', msg_type).decode('utf-8')
    msg = '%s%s%s%s' % (magic_string, payload_length, msg_type, payload)
    msg = msg.encode('utf-8')
    return msg
```

# Send and Receiving Messages

Sending a message is quite trivial:

```Python
def send(self, msg):
    self.sock.sendall(msg)
```

However, receiving messages is a bit trickier. Information is transferred through a socket as a continuous stream, and while it may contain discrete messages, there is no in-built mechanism to segment them. For that reason, we receive messages in two stages. First, we download those three standard standard elements that begin a message, which I'll term the header. Because the header takes a standard form, we can know exactly how much to download by measuring how long one would be. The above lines should go somewhere near the top of your script:

```Python
magic_string = 'i3-ipc'j
struct_header = '<%dsII' % len(magic_string.encode('utf-8'))
struct_header_size = struct.calcsize(struct_header)
```

Now that we know how large a message header will be, we can plug that length into a call to receive data:

```Python
header = self.sock.recv(struct_header_size)
```

We then make use of the struct module to unpack the header, using the blueprint we established above:

```Python
header = struct.unpack(struct_header, header)
```

The unpack method returns a triple tuple, which we then split into its constituent parts:

```Python
magic_string, msg_length, msg_type = self.process_header(header)
```

The reason that the payload length is included in the header is that it tells us exactly how much more we need to receive from the socket to complete the message. We can now use that measurement go about gathering the remaining data:

```Python
payload = self.sock.recv(msg_length)
\\# if the above didn't work, keep receiving from socket until we get the rest
while len(payload) < msg_length:
	data += self.sock.recv(msg_length-len(payload))
```

As i3 sends the payload in JSON format, all that's left to do is convert it into something python can work with:

```Python
payload = data.decode('utf-8')
payload = json.loads(payload)
```

Altogether, we'll end up with methods looking something like this:

```Python
def send(self, msg_type, payload):
    msg = self.format_msg(msg_type, payload)
    self.sock.sendall(msg)

def receive(self):
    header = self.sock.recv(struct_header_size)
    header = struct.unpack(struct_header, header)
    magic_string, msg_length, msg_type = header
    data = self.sock.recv(msg_length)
    while len(data) < msg_length:
    data += self.sock.recv(msg_length-len(data))
    data = self.deformat_msg(data)
    data = data.decode('utf-8')
    data = json.loads(payload)
    return data

def get(self, msg_type, payload):
    self.send(msg_type, payload)
    data = self.receive()
    return data
```

You'll notice that I've added an extra "get" method, because you'll almost always want to send and receive in succession.


# Subscribing

This is where the multiple sockets comes in. If you use the same socket to receive event messages as you do to make queries, from time to time you will send a request for information, and get an event message before the response, which will confuse your script. Using one socket to receive event messages and another for queries is a lot easier than building in a mechanism to handle unexpected messages, and the former is indeed recommended by the i3 IPC docs.

```Python
sub_sock = Socket()
data_sock = Socket()
```

Subscribing is pretty simple. You send i3 a message with a message code of 2, where the payload is json-formatted list of event types. The most basic way to that with what we've covered so far is this:

```Python
payload = json.dumps(['workspace'])
subscription = sub_sock.get(2, payload)
```

We use get and fill the "subscription" variable because a subscription attempt returns a message saying whether or not the subscription was successful or a parse error ocurred. You'll at least want to retreive to get it out of the way, if not to evaluate it in some way.

# Listen For Events

With a subscription secured, event messages will be incoming, and we'll need to be listening for them. A "listen" function like the following will have to be added to the Socket class. It loops forever trying to receive a message, and simply continues if a reception attempt times out (which it will if there's nothing to receive, and the majority of the time there won't be).

```Python
def listen(self, callback=False):
	while True:
		try:
			event = self.receive()
			if callback:
				callback(event)
			else:
				print(event)
	except socket.timeout:
		continue
```

# Get and Print Workspace Info

The problem with workspace events is that they only report information relevent to the current workspace. To get an overview of all workspaces we need to make a separate query for workspace information. You'll notice that the listen method takes an optional callback function. It's this function that we'll use to query for workspace info, format it, and then print it for external use (in this case lemonbar). You'll notice it queries using the "data_sock" socket for reasons discussed above.

```Python
def print_workspaces(event):
	data = data_sock.get(1, '')[1]
	#pprint.pprint(event)
	if event[1]['change'] not in  ['focus', 'init']:
		return
	output = "3"
	for workspace in data:
	if workspace["focused"]:
		w = "foc"
	elif workspace["urgent"]:
		w = 'urg'
	else:
		w = 'unf'
	w += workspace['name']
	output += ' %s' % w
	print(output)
```

The above function gets a bit of information about the workspaces, labels them with strings I use for formatting in my lemonbar script, and prints everything in a single string. If we call the listen method with this function as an argument, it will conveniently print a new line every time something changes.

```Python
sub_sock.subscribe('workspace', print_workspaces)
```

# In Sum

With that we have working script that gets and receives messages from i3, subscribes to workspace events, listens for event messages, and then runs a callback function to get and print the workspace info we want. While specific to my use-case, this example covers all of the basics of communicating with i3 with Python. With a little research in the i3-IPC documentation it should be easy to adapt or expand it to other tasks.

My own script is only slightly more complex than this, and as of yet does not have  general-purpose functionality. I do, however, intend to incrementally expand it as my demands grow, and I may end up polishing it and offering it as a fully featured library.
