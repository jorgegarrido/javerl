javerl
======

Erlang-Java Interface: interact erlang/java

Clone the project from:
				https://
      
Into javerl:
		$ cd javerl

Compile javerl:
		$ make compile

Into examples:
		$ cd examples

Let's compile the Javerl.java:
		$ javac Javerl.java
      
To test the connection between erlang and java, start an erlang 
shell with the code path to javerl:
		$ erl -pa ../ebin
      
Start the application javerl inside erlang shell:
		1> application:start(javerl).
		ok.

Now an erlang socket is ready to receive messages from Java, the class Javerl.java
send a simple message to this socket.

To send a message from java, pass the correct arguments to the java class:
		$ java Javerl 127.0.0.1 2575 hello
      
The first argument is the ip which the erlang application was started (javerl).
The second argumetn is the socket's port
The third argument is the message sended to javerl

Now you can view activity on the erlang shell:
		2>
		=INFO REPORT==== XX-xxx-XXXX::XX:XX:XX ===
		Incoming message : "hello" 
		Socket #Port<0.673> closed

And the response when the java class Javerl was executed is:
		Available bytes: 2

Since the java class prints the total bytes return by erlang!

Also you can configure the port in the file ebin/javerl.app in the env section.
