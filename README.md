javerl
======

Erlang-Java Interface: interact erlang/java

Clone the project from:

				$ git clone https://github.com/jorgegarrido/javerl.git
      
Into javerl:

				$ cd javerl

Compile javerl:

				$ make compile

Let's start the application and compile the java example in 
java_src:

				$ make example

Now an erlang socket is ready to receive messages from Java, the class Javerl.java
send a simple message to this socket.

To send a message from java move to java_src directory and
pass the correct arguments to the java class:
				
				$ cd java_src
				$ java Javerl 127.0.0.1 2575 hello
      
The first argument is the ip which the erlang application was started (javerl).

The second argumetn is the socket's port.

The third argument is the message sended to javerl.

Now you can view activity on the erlang shell:
		
				2>
				=INFO REPORT==== XX-xxx-XXXX::XX:XX:XX ===
				Incoming message : "hello" 
				Socket closed

And the response when the java class Javerl was executed is:
		
				$ available:14
				  response erlang: erlang rocks!!

Since the java class prints the total bytes and the message returned by erlang!

Also you can configure the port and reply in the file ebin/javerl.app in the env section.
