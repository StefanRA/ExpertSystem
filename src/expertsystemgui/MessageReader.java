package expertsystemgui;

import java.io.IOException;
import java.io.InputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.SwingUtilities;

public class MessageReader extends Thread {
    ServerSocket serverSocket;
    volatile Socket socket = null;
    PrologConnection connection;

    public synchronized void setSocket(Socket socket){
        this.socket = socket;
        notify();
    }    
    
    public synchronized Socket getSocket() throws InterruptedException
    {
        if (socket==null){
            wait(); //waits until a socket is set
        }
        return socket;
    }
    
    
    public MessageReader(PrologConnection connection, ServerSocket serverSocket) throws IOException{
        this.serverSocket = serverSocket;
        this.connection = connection;
    }
    
    @Override
    public void run(){
        try {
            Socket localSocket = serverSocket.accept();
            setSocket(localSocket);
            
            InputStream inputStream = localSocket.getInputStream();
            
            int chr;
            String str = "";
            while((chr = inputStream.read())!=-1) {
                str += (char)chr;
                if(chr=='\n'){
                    final String finalString = str;
                    str="";
                    SwingUtilities.invokeLater(new Runnable() {
                        public void run(){ 
                            
                            Window window = connection.getWindow();
                            
                            window.getOutputTextArea().append(finalString); 
                            String text = finalString.trim();
                            //if message represents a question
                            if(text.length()>2 && text.charAt(0)=='i'&& text.charAt(1)=='(' && text.charAt(text.length()-1)==')')
                            {
                                String attributeAndQuestion = text.substring(2, text.length()-1);
                                String[] s = attributeAndQuestion.split("~");
                                String attribute = s[0];
                                String question = s[1].substring(1, s[1].length()-1);
                                String options = s[2];
                                
                                window.setQuestionAndOptions(question, options, attribute);
                            }
                            //if message represents a solution
                            if(text.length()>2 && text.charAt(0)=='s'&& text.charAt(1)=='(' && text.charAt(text.length()-1)==')')
                            {
                                String solution=text.substring(2, text.length()-1);
                                window.setSolution(solution);
                            }
                            //if message is part of a solution proof
                            if(text.length()>2 && text.substring(0,3).equals("dem")&& text.charAt(3)=='(')
                            {
                                String proofText = text.substring(4);
                                window.setProofText(proofText+"\n");
                            }
                        }

                    });
                }
            }
            
            
        } catch (IOException ex) {
            Logger.getLogger(MessageReader.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
}
