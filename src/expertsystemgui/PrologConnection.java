package expertsystemgui;

import java.io.IOException;
import java.io.PipedOutputStream;
import java.io.PrintStream;
import java.net.ServerSocket;

public class PrologConnection {
    final String sicstusPath = "D:\\Facultate\\An III\\Sem II\\SistExpert\\ia\\SICStus Prolog 4.0.2\\bin\\spwin.exe";
    final String filename = "sistem_expert.pl";
    final String goal = "guiEntryPoint.";

    Process sictusProcess;
    MessageSender messageSender;
    MessageReader messageReader;
    Window window;
    int port;
    
    
    public Window getWindow(){
        return window;
    }
    
    public PrologConnection(int port, Window window) throws IOException, InterruptedException{
//        InputStream processIs, processStreamErr;
        this.port = port;
        this.window = window;
        ServerSocket serverSocket = new ServerSocket(port);
        messageReader = new MessageReader(this, serverSocket);
        messageReader.start();
        messageSender = new MessageSender(messageReader);
        messageSender.start();
        
        
        Runtime runTime = Runtime.getRuntime();
        
        String command = sicstusPath+" -f -l "+filename+" --goal "+goal+" -a "+port;
        
        sictusProcess = runTime .exec(command);
        
//        //InputStream-ul din care citim ce scrie procesul
//        processIs=sictusProcess.getInputStream();
//        //stream-ul de eroare
//        processStreamErr=sictusProcess.getErrorStream();

    }
    
    void opresteProlog() throws InterruptedException{
        PipedOutputStream pipedOutputStream= this.messageSender.getPipedOutputStream();
        PrintStream printStream=new PrintStream(pipedOutputStream);
        printStream.println("exit.");
        printStream.flush();
    }
}
