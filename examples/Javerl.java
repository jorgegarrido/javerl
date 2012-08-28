import java.io.*;
import java.net.*;

public class Javerl {

  private String host;
  private int port;

  public Javerl(String host, int port) {
     this.host = host;
     this.port = port;
  }

  public static void main(String[] args) {
      try {
        byte[] message = args[2].getBytes("US-ASCII");
        Javerl socket = new Javerl(args[0], Integer.parseInt(args[1]));
        socket.send_get_message(message);
      } catch (Exception ex) {
        System.out.println(ex.getMessage());
      }
  }

  public void send_get_message(byte message[]) {
     try {
       Socket socket = new Socket(host, port);
       OutputStream output = socket.getOutputStream();
       InputStream input = socket.getInputStream();

      // Send message
      output.write(message);
      output.flush();

      // Get the result
      System.out.println("Available bytes: " + input.available());
      socket.close();
      return;
    } catch (Exception ex) {
       System.out.println(ex.getMessage());
    }
  }
 
}
