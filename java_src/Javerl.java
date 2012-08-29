// ----------------------------------------------
//
// Jorge Garrido <jorge.garrido@morelosoft.com>
//
// Javerl.erl
//
// ----------------------------------------------

import java.io.*;
import java.net.*;
import java.util.Arrays;

public class Javerl {

  private String host;
  private int port;

  /**
   *
   * @name Javerl
   * @description constructor for Javerl class
   *
   * @param host The ip or hostname where erlang socket resides
   * @param port The port where erlang socket resides
   *
   */
  public Javerl(String host, int port) {
     this.host = host;
     this.port = port;
  }

  /**
   *
   * @name main
   * @description main method for this class
   *
   * @param args The args passed on command line arguments
   *
   */
  public static void main(String[] args) {
      try {
        byte[] message = args[2].getBytes("US-ASCII");
        Javerl socket = new Javerl(args[0], Integer.parseInt(args[1]));
        socket.send_get_message(message);
      } catch (Exception ex) {
        System.out.println(ex.getMessage());
      }
  }

  /**
   *
   * @name send_get_message
   * @description sends and gets a message to/from erlang socket
   *
   * @param message The message in bytes to be sent 
   *
   */
  public void send_get_message(byte message[]) {
     try {
       Socket socket = new Socket(host, port);
       OutputStream output = socket.getOutputStream();
       InputStream input = socket.getInputStream();

      // Send message
      output.write(message);
      output.flush();

      // Retrieve the response as string
      Thread.sleep(10000);
      int available = input.available();
      System.out.println("available:" + available);
      byte[] read = new byte[available];
      int x = 0;
      int ret = 0;
      while (x < available) {
         ret = input.read();
         read[x] = (byte) ret;
         x ++;
      }
      String bytes = new String(read, 0, 0, read.length);
      System.out.println("erlang response: " + bytes);

      socket.close();
      return;
    } catch (Exception ex) {
       System.out.println(ex.getMessage());
    }
  }
}
