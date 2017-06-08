package expertsystemgui;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.io.PrintStream;
import java.net.Socket;
import java.util.logging.Level;
import java.util.logging.Logger;

public class MessageSender extends Thread{
    Socket socket;
    MessageReader messageReader;
    volatile PipedOutputStream pipedOutputStream = null;
    volatile PipedInputStream pipedInputStream;
    OutputStream outputStream;
    volatile boolean done = false;

    public final synchronized void setPipedOutputStream(PipedOutputStream pipedOutputStream){
        this.pipedOutputStream = pipedOutputStream;
        notify();
    }
    
    public synchronized PipedOutputStream getPipedOutputStream() throws InterruptedException{
        if(pipedOutputStream==null){
            wait();
        }
        return pipedOutputStream;
    }    
    
    
    public MessageSender(MessageReader messageReader) throws IOException{
        this.messageReader = messageReader;
        pipedInputStream = new PipedInputStream();
        setPipedOutputStream(new PipedOutputStream(pipedInputStream));

    }
    
    public void sendMessageToExpertSystem(String message) throws InterruptedException{
        PipedOutputStream pipedOutputStream= getPipedOutputStream();
        PrintStream printStream = new PrintStream(pipedOutputStream);
        printStream.println(message + ".");
        printStream.flush();
    }

    public void sendStringToExpertSystem(String message) throws InterruptedException{
        PipedOutputStream pipedOutputStream= getPipedOutputStream();
        PrintStream printStream=new PrintStream(pipedOutputStream);
        printStream.println(message);
        printStream.flush();
    }
    
    public void run(){
        try {
            socket = messageReader.getSocket();
            outputStream = socket.getOutputStream();
            int chr;
            while((chr = pipedInputStream.read()) != -1){
                outputStream.write(chr);
            }
            System.out.println("done");
            
        
        } catch (IOException | InterruptedException ex) {
            Logger.getLogger(MessageSender.class.getName()).log(Level.SEVERE, null, ex);
        }
        
    }
}
