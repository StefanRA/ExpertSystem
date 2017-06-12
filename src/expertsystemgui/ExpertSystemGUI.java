package expertsystemgui;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

public class ExpertSystemGUI {
    private static final int PORT=5010;
    private static PrologConnection connection;
    private static Window window;
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        window = new Window("Expert System");
        initializeConnection();
        initializeWindow();
    }

    private static void initializeConnection(){
        try{
            connection = new PrologConnection(PORT, window);
        }
        catch (IOException ex) {
            Logger.getLogger(ExpertSystemGUI.class.getName()).log(Level.SEVERE, null, ex);
            System.out.println("Cannot initialize connection!");
        } 
        catch (InterruptedException ex) {
            Logger.getLogger(ExpertSystemGUI.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
    private static void initializeWindow() {
        window.setConnection(connection);
        window.setVisible(true);
        window.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                try {
                    window.connection.opresteProlog();                        
                    window.connection.messageSender.done = true;
                    window.updateLastAccessFile();
                } catch (InterruptedException ex) {
                    Logger.getLogger(ExpertSystemGUI.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        });  
    }
    
}
