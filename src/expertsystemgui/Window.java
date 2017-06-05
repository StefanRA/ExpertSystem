package expertsystemgui;

import java.awt.FlowLayout;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.DefaultListModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JRadioButton;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import org.joda.time.DateTime;
import org.joda.time.Days;
import org.joda.time.Hours;
import org.joda.time.Minutes;
import org.joda.time.Seconds;

public class Window extends javax.swing.JFrame {

    /**
     * Creates new form Window
     * @param titlu
     */
    PrologConnection connection;
    LinkedHashMap<String, List<String>> QandA = new LinkedHashMap<>();
    List<String> solutions = new ArrayList<>();
    
    public Window(String title) {
        super(title);
        initComponents();
        setExtendedState(java.awt.Frame.MAXIMIZED_BOTH);
        readLastAccessFile();
    }
    
    private void readLastAccessFile() {
        try(BufferedReader bufferedReader = new BufferedReader(new FileReader(new File("lastAccess.txt")))){
            String s;
            s = bufferedReader.readLine();
            DateTime lastAccessDate = new DateTime(s);
            DateTime now = DateTime.now();
            int days = Days.daysBetween(lastAccessDate, now).getDays();
            int hours = Hours.hoursBetween(lastAccessDate, now).getHours() % 24;
            int minutes = Minutes.minutesBetween(lastAccessDate, now).getMinutes() % 60;
            int seconds = Seconds.secondsBetween(lastAccessDate, now).getSeconds() % 60;
            String message = "Ultima accesare a fost acum " +
                             (days != 0 ?(days + " zile, ") : "") +
                             (hours != 0 ? (hours + " ore, ") : "") + 
                             (minutes != 0 ? (minutes + " minute si ") : "") + 
                             (seconds+" secunde.");
            greetingMessageLabel.setText(message);
            updateLastAccessFile();
        } 
        catch (IOException ex) {
            String message = "Bine ati venit!";
            greetingMessageLabel.setText(message);
            updateLastAccessFile();
        }
        finally{
            
        }
    }
    
    private void updateLastAccessFile(){
        try (PrintWriter writer = new PrintWriter("lastAccess.txt", "UTF-8")) {
            DateTime now = DateTime.now();
            writer.println(now.toString());
        }
        catch(IOException e){
            e.printStackTrace();
        }
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        questionsRadioButtonGroup = new javax.swing.ButtonGroup();
        commandPanel = new javax.swing.JPanel();
        loadRulesButton = new javax.swing.JButton();
        rulesFilenameTextField = new javax.swing.JTextField();
        rulesFilenameLabel = new javax.swing.JLabel();
        consultButton = new javax.swing.JButton();
        resetSystemButton = new javax.swing.JButton();
        greetingMessageLabel = new javax.swing.JLabel();
        solutionsFilenameLabel = new javax.swing.JLabel();
        solutionsFilenameTextField = new javax.swing.JTextField();
        questionsPanel = new javax.swing.JPanel();
        backToMenuButton = new javax.swing.JButton();
        optionsPanel = new javax.swing.JPanel();
        questionLabel = new javax.swing.JLabel();
        certaintyFactorComboBox = new javax.swing.JComboBox<>();
        certaintyFactorLabel = new javax.swing.JLabel();
        questionsRadioButtonPanel = new javax.swing.JPanel();
        answersScrollPane = new javax.swing.JScrollPane();
        answersTextArea = new javax.swing.JTextArea();
        solutionsScrollPane = new javax.swing.JScrollPane();
        solutionsList = new javax.swing.JList<>();
        proofScrollPane = new javax.swing.JScrollPane();
        proofTextPane = new javax.swing.JTextPane();
        outputAreaPanel = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        outputTextArea = new javax.swing.JTextArea();

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
        setMinimumSize(new java.awt.Dimension(800, 600));
        setPreferredSize(new java.awt.Dimension(1200, 700));

        loadRulesButton.setText("Load rules");
        loadRulesButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                loadRulesButtonActionPerformed(evt);
            }
        });

        rulesFilenameTextField.setText("Rules.txt");

        rulesFilenameLabel.setText("Rules filename:");

        consultButton.setText("Let's find out your holiday destination");
        consultButton.setEnabled(false);
        consultButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                consultButtonActionPerformed(evt);
            }
        });

        resetSystemButton.setText("Reset system");
        resetSystemButton.setEnabled(false);
        resetSystemButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                resetSystemButtonActionPerformed(evt);
            }
        });

        greetingMessageLabel.setText("Mesaj de bun venit");

        solutionsFilenameLabel.setText("Solutions filename:");

        solutionsFilenameTextField.setText("SolutionInfo.txt");

        javax.swing.GroupLayout commandPanelLayout = new javax.swing.GroupLayout(commandPanel);
        commandPanel.setLayout(commandPanelLayout);
        commandPanelLayout.setHorizontalGroup(
            commandPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, commandPanelLayout.createSequentialGroup()
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(consultButton, javax.swing.GroupLayout.PREFERRED_SIZE, 251, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
            .addGroup(commandPanelLayout.createSequentialGroup()
                .addGap(40, 40, 40)
                .addComponent(loadRulesButton, javax.swing.GroupLayout.PREFERRED_SIZE, 307, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
            .addGroup(commandPanelLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(rulesFilenameLabel, javax.swing.GroupLayout.PREFERRED_SIZE, 95, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(rulesFilenameTextField, javax.swing.GroupLayout.PREFERRED_SIZE, 67, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGroup(commandPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(commandPanelLayout.createSequentialGroup()
                        .addGap(45, 45, 45)
                        .addComponent(solutionsFilenameLabel)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(solutionsFilenameTextField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 41, Short.MAX_VALUE)
                        .addComponent(resetSystemButton, javax.swing.GroupLayout.PREFERRED_SIZE, 147, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addContainerGap(157, Short.MAX_VALUE))
                    .addGroup(commandPanelLayout.createSequentialGroup()
                        .addGap(299, 299, 299)
                        .addComponent(greetingMessageLabel, javax.swing.GroupLayout.PREFERRED_SIZE, 246, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))))
        );
        commandPanelLayout.setVerticalGroup(
            commandPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(commandPanelLayout.createSequentialGroup()
                .addComponent(greetingMessageLabel)
                .addGap(10, 10, 10)
                .addGroup(commandPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(rulesFilenameTextField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(rulesFilenameLabel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(solutionsFilenameLabel)
                    .addComponent(solutionsFilenameTextField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(resetSystemButton))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(loadRulesButton)
                .addGap(18, 18, 18)
                .addComponent(consultButton))
        );

        getContentPane().add(commandPanel, java.awt.BorderLayout.NORTH);

        questionsPanel.setVisible(false);

        backToMenuButton.setText("Back to menu");
        backToMenuButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                backToMenuButtonActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout optionsPanelLayout = new javax.swing.GroupLayout(optionsPanel);
        optionsPanel.setLayout(optionsPanelLayout);
        optionsPanelLayout.setHorizontalGroup(
            optionsPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 133, Short.MAX_VALUE)
        );
        optionsPanelLayout.setVerticalGroup(
            optionsPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 100, Short.MAX_VALUE)
        );

        questionLabel.setText("Question");

        certaintyFactorComboBox.setModel(new javax.swing.DefaultComboBoxModel<>(new String[] { "100", "95", "90", "85", "80", "75", "70", "65", "60", "55", "50", "45", "40", "35", "30", "25", "20", "10", "5", "0" }));

        certaintyFactorLabel.setText("Factor de certitudine");

        questionsRadioButtonPanel.setLayout(new javax.swing.BoxLayout(questionsRadioButtonPanel, javax.swing.BoxLayout.Y_AXIS));

        answersScrollPane.setVisible(false);

        answersTextArea.setColumns(20);
        answersTextArea.setLineWrap(true);
        answersTextArea.setRows(5);
        answersScrollPane.setViewportView(answersTextArea);

        solutionsScrollPane.setPreferredSize(new java.awt.Dimension(258, 100));

        solutionsList.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        solutionsList.setVisibleRowCount(-1);
        solutionsList.addListSelectionListener(new javax.swing.event.ListSelectionListener() {
            public void valueChanged(javax.swing.event.ListSelectionEvent evt) {
                solutionsListValueChanged(evt);
            }
        });
        solutionsScrollPane.setViewportView(solutionsList);

        proofScrollPane.setViewportView(proofTextPane);

        javax.swing.GroupLayout questionsPanelLayout = new javax.swing.GroupLayout(questionsPanel);
        questionsPanel.setLayout(questionsPanelLayout);
        questionsPanelLayout.setHorizontalGroup(
            questionsPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(questionsPanelLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(solutionsScrollPane, javax.swing.GroupLayout.PREFERRED_SIZE, 277, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(proofScrollPane)
                .addGap(18, 18, 18)
                .addComponent(answersScrollPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
            .addGroup(questionsPanelLayout.createSequentialGroup()
                .addGroup(questionsPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(questionsPanelLayout.createSequentialGroup()
                        .addComponent(optionsPanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 138, Short.MAX_VALUE)
                        .addGroup(questionsPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(certaintyFactorLabel)
                            .addComponent(backToMenuButton)
                            .addGroup(questionsPanelLayout.createSequentialGroup()
                                .addGap(21, 21, 21)
                                .addComponent(certaintyFactorComboBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
                        .addGap(125, 125, 125))
                    .addGroup(questionsPanelLayout.createSequentialGroup()
                        .addGap(82, 82, 82)
                        .addComponent(questionLabel, javax.swing.GroupLayout.PREFERRED_SIZE, 293, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(98, 98, 98)))
                .addComponent(questionsRadioButtonPanel, javax.swing.GroupLayout.PREFERRED_SIZE, 217, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(29, 29, 29))
        );
        questionsPanelLayout.setVerticalGroup(
            questionsPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, questionsPanelLayout.createSequentialGroup()
                .addContainerGap(14, Short.MAX_VALUE)
                .addGroup(questionsPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, questionsPanelLayout.createSequentialGroup()
                        .addComponent(questionLabel)
                        .addGap(11, 11, 11)
                        .addGroup(questionsPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(optionsPanel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addGroup(questionsPanelLayout.createSequentialGroup()
                                .addComponent(certaintyFactorLabel)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(certaintyFactorComboBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addGap(18, 18, 18)
                                .addComponent(backToMenuButton)))
                        .addGap(56, 56, 56))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, questionsPanelLayout.createSequentialGroup()
                        .addComponent(questionsRadioButtonPanel, javax.swing.GroupLayout.PREFERRED_SIZE, 163, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)))
                .addGroup(questionsPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(proofScrollPane, javax.swing.GroupLayout.PREFERRED_SIZE, 164, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(answersScrollPane, javax.swing.GroupLayout.PREFERRED_SIZE, 164, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(solutionsScrollPane, javax.swing.GroupLayout.PREFERRED_SIZE, 127, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(33, 33, 33))
        );

        getContentPane().add(questionsPanel, java.awt.BorderLayout.CENTER);

        outputTextArea.setColumns(20);
        outputTextArea.setLineWrap(true);
        outputTextArea.setRows(5);
        jScrollPane1.setViewportView(outputTextArea);

        javax.swing.GroupLayout outputAreaPanelLayout = new javax.swing.GroupLayout(outputAreaPanel);
        outputAreaPanel.setLayout(outputAreaPanelLayout);
        outputAreaPanelLayout.setHorizontalGroup(
            outputAreaPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, outputAreaPanelLayout.createSequentialGroup()
                .addGap(245, 245, 245)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 251, Short.MAX_VALUE)
                .addGap(246, 246, 246))
        );
        outputAreaPanelLayout.setVerticalGroup(
            outputAreaPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(outputAreaPanelLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 82, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(25, Short.MAX_VALUE))
        );

        getContentPane().add(outputAreaPanel, java.awt.BorderLayout.SOUTH);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void loadRulesButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_loadRulesButtonActionPerformed
        Window.SHOW_SOLUTIONS = false;
        String rulesFilename = rulesFilenameTextField.getText();
        String solutionsFilename = solutionsFilenameTextField.getText();
        consultButton.setEnabled(true);
        resetSystemButton.setEnabled(true);
        String dir = System.getProperty("user.dir");
        dir=dir.replace("\\", "/");
        try {
            connection.messageSender.sendMessageToExpertSystem("set_current_directory('"+dir+"')");
            connection.messageSender.sendMessageToExpertSystem("load('"+rulesFilename+"','"+solutionsFilename+"')");
            
        } catch (InterruptedException ex) {
            Logger.getLogger(Window.class.getName()).log(Level.SEVERE, null, ex);
        }
        
    }//GEN-LAST:event_loadRulesButtonActionPerformed

    private void consultButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_consultButtonActionPerformed
        switchToConsultView();
        try {
            connection.messageSender.sendMessageToExpertSystem("command(consult)");
        
        } catch (InterruptedException ex) {
            Logger.getLogger(Window.class.getName()).log(Level.SEVERE, null, ex);
        }
    }//GEN-LAST:event_consultButtonActionPerformed

    private void backToMenuButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_backToMenuButtonActionPerformed
        // TODO add your handling code here:
        Window.SHOW_SOLUTIONS = false;
        //switchToMenuView();
    }//GEN-LAST:event_backToMenuButtonActionPerformed

    private void resetSystemButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_resetSystemButtonActionPerformed
        // TODO add your handling code here:    
        try {
            connection.messageSender.sendMessageToExpertSystem("command(reset)");
        
        } catch (InterruptedException ex) {
            Logger.getLogger(Window.class.getName()).log(Level.SEVERE, null, ex);
        }
        
        this.consultButton.setEnabled(true);
        this.questionsRadioButtonPanel.removeAll();
        this.answersScrollPane.setVisible(false);
        this.proofTextPane.setText("");
        
        DefaultListModel listModel = (DefaultListModel) solutionsList.getModel();
        listModel.removeAllElements();
        
        solutions.clear();
        QandA.clear();
        this.revalidate();
        this.repaint();
    }//GEN-LAST:event_resetSystemButtonActionPerformed

    private void solutionsListValueChanged(javax.swing.event.ListSelectionEvent evt) {//GEN-FIRST:event_solutionsListValueChanged
        // TODO add your handling code here:
        if (evt.getValueIsAdjusting() == false) {
            
            int index = solutionsList.getSelectedIndex();
            if (index == -1)
                return;
            
            String selectedSolution = solutionsList.getSelectedValue();
            
            getProofForSolution(selectedSolution);
        }
    }//GEN-LAST:event_solutionsListValueChanged

    private void getProofForSolution(String selectedSolution) {
        try {
            selectedSolution = selectedSolution.endsWith("cu fc 100") ?
                               selectedSolution.substring(0,selectedSolution.length()-10) :
                               selectedSolution.substring(0,selectedSolution.length()-9);
            
            String[] words = selectedSolution.split(" ");
            String message = String.join(",", words);
            message = "how([" + message + "])";
            
            proofTextPane.setText("");
            connection.messageSender.sendMessageToExpertSystem(message);
            proofScrollPane.setVisible(true);
            questionsPanel.revalidate();
            questionsPanel.repaint();
        }
        catch (InterruptedException ex) {
            Logger.getLogger(Window.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    private void switchToConsultView() {
        //this.commandPanel.setVisible(false);
        this.questionsPanel.setVisible(true);
        this.backToMenuButton.setVisible(false);
        this.consultButton.setEnabled(false);
        
        this.optionsPanel.setVisible(true);
        this.questionLabel.setVisible(true);
        this.certaintyFactorComboBox.setVisible(true);
        this.certaintyFactorLabel.setVisible(true);
        
        this.revalidate();
        this.repaint();
    }                                             

    private void switchToMenuView() {
        this.optionsPanel.removeAll();
        this.optionsPanel.repaint();
        this.optionsPanel.revalidate();
        
        this.questionLabel.setText("");
        
        this.commandPanel.setVisible(true);
        this.questionsPanel.setVisible(false);
    }                                            

    private void switchToSolutionView() {
        this.optionsPanel.setVisible(false);
        this.questionLabel.setVisible(false);
        this.certaintyFactorComboBox.setVisible(false);
        this.certaintyFactorLabel.setVisible(false);
    }

    
     private void optionButtonActionPerformed(java.awt.event.ActionEvent evt) {
         
        int certaintyFactor = Integer.parseInt(certaintyFactorComboBox.getSelectedItem().toString());
        String answer = ((JButton)(evt.getSource())).getText() + " fc " + certaintyFactor;
        try {
            connection.messageSender.sendStringToExpertSystem(answer);
        
        } catch (InterruptedException ex) {
            Logger.getLogger(Window.class.getName()).log(Level.SEVERE, null, ex);
        }
        
        SaveQuestionAndAnswerAndAddRadioButton(evt.getActionCommand(), answer);
    }

    private void SaveQuestionAndAnswerAndAddRadioButton(String attributeQuestion, String answer) {
        String[] s = attributeQuestion.split("~");
        String attribute = s[0];
        String question = s[1];
        addAtribute(attribute, question, answer);
        
        JRadioButton radioButton = new JRadioButton(attribute);
        radioButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                radioButtonActionPerformed(evt);
            }
        });
        questionsRadioButtonGroup.add(radioButton);
        questionsRadioButtonPanel.add(radioButton);
        questionsRadioButtonPanel.revalidate();
        questionsRadioButtonPanel.repaint();
    }
    
    public void addAtribute(String attribute, String question, String answer){
        List<String> list = Arrays.asList(question, answer);
        QandA.put(attribute, list);
    }
    
    
    private void radioButtonActionPerformed(java.awt.event.ActionEvent evt) {
        String attribute = ((JRadioButton)(evt.getSource())).getText();
        List<String> questionAndAnswer = QandA.get(attribute);
        String question = "Q: " + questionAndAnswer.get(0);
        String answer = "A: " + questionAndAnswer.get(1);
        answer = answer.endsWith("fc 100") ? answer.substring(0,answer.length()-7) : answer;
        
        answersTextArea.setText(question + "\n" + answer);
        answersScrollPane.setVisible(true);
        questionsPanel.revalidate();
        questionsPanel.repaint();
    } 
    
    
    
    /**
     * @param args the command line arguments
     */
    public static void main(String args[]) {
        /* Set the Nimbus look and feel */
        //<editor-fold defaultstate="collapsed" desc=" Look and feel setting code (optional) ">
        /* If Nimbus (introduced in Java SE 6) is not available, stay with the default look and feel.
         * For details see http://download.oracle.com/javase/tutorial/uiswing/lookandfeel/plaf.html 
         */
        try {
            for (javax.swing.UIManager.LookAndFeelInfo info : javax.swing.UIManager.getInstalledLookAndFeels()) {
                if ("Nimbus".equals(info.getName())) {
                    javax.swing.UIManager.setLookAndFeel(info.getClassName());
                    break;
                }
            }
        } catch (ClassNotFoundException | InstantiationException | IllegalAccessException | javax.swing.UnsupportedLookAndFeelException ex) {
            java.util.logging.Logger.getLogger(Window.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        }
        //</editor-fold>
        //</editor-fold>
        
        //</editor-fold>
        //</editor-fold>

        /* Create and display the form */
        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
                new Window("Verificare").setVisible(true);
                
            }
        });
    }

    public javax.swing.JTextArea getOutputTextArea(){
        return outputTextArea;
    }
    
    public void setConnection(PrologConnection connection){
        this.connection = connection;
    }
    public void setQuestionAndOptions(String question, String optionsString, String attribute){
        
        this.questionLabel.setText("<html><body style='width:100%'>"+question+"</html>");
        this.questionsPanel.repaint();
        
        this.optionsPanel.removeAll();
        this.optionsPanel.setLayout(new FlowLayout());
        
        optionsString = optionsString.trim();
        String[] options = optionsString.split(" ");
        
        for (int i = 0; i < options.length; i++) {
            
            String iconPath = "icons/icon" + (i+1) + ".png";
            ImageIcon imageIcon = createImageIcon(iconPath);
            
            JButton optionButton = new JButton(options[i], imageIcon); 
            String s = attribute + "~" + question;
            optionButton.setActionCommand(s);
            optionButton.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    optionButtonActionPerformed(evt);
                }
            });
            this.optionsPanel.add(optionButton);
        }
        this.optionsPanel.repaint();
        this.optionsPanel.revalidate();
    } 

     /** Returns an ImageIcon, or null if the path was invalid. */
    private ImageIcon createImageIcon(String path) {
        java.net.URL imgURL = getClass().getResource(path);
        if (imgURL != null) {
            return new ImageIcon(imgURL);
        } else {
            System.err.println("Couldn't find file: " + path);
            return null;
        }
    }
     
    public void setSolution(String solution){
         if(solution.equals("done")){
             addSolutionsToJList();
             return;
         }
        solutions.add(solution);
        
        switchToSolutionView();
    } 

    private void addSolutionsToJList() {
        DefaultListModel<String> listModel = new DefaultListModel<>();
        for(String s : solutions){
            listModel.addElement(s);
        }
        solutionsList.setModel(listModel);
    }
    
    public void setProofText(String text){
        text = text.charAt(0) == ' ' ? text : "\n"+text;
        text = text.startsWith("regula") ? "\n"+text : text;
        Document doc = proofTextPane.getDocument();
        try {
            doc.insertString(doc.getLength(), text, null);
        } 
        catch (BadLocationException ex) {
            Logger.getLogger(Window.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
    public static boolean SHOW_SOLUTIONS = false;
    
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JScrollPane answersScrollPane;
    private javax.swing.JTextArea answersTextArea;
    private javax.swing.JButton backToMenuButton;
    private javax.swing.JComboBox<String> certaintyFactorComboBox;
    private javax.swing.JLabel certaintyFactorLabel;
    private javax.swing.JPanel commandPanel;
    private javax.swing.JButton consultButton;
    private javax.swing.JLabel greetingMessageLabel;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JButton loadRulesButton;
    public javax.swing.JPanel optionsPanel;
    private javax.swing.JPanel outputAreaPanel;
    private javax.swing.JTextArea outputTextArea;
    private javax.swing.JScrollPane proofScrollPane;
    private javax.swing.JTextPane proofTextPane;
    public javax.swing.JLabel questionLabel;
    private javax.swing.JPanel questionsPanel;
    private javax.swing.ButtonGroup questionsRadioButtonGroup;
    private javax.swing.JPanel questionsRadioButtonPanel;
    private javax.swing.JButton resetSystemButton;
    private javax.swing.JLabel rulesFilenameLabel;
    private javax.swing.JTextField rulesFilenameTextField;
    private javax.swing.JLabel solutionsFilenameLabel;
    private javax.swing.JTextField solutionsFilenameTextField;
    private javax.swing.JList<String> solutionsList;
    private javax.swing.JScrollPane solutionsScrollPane;
    // End of variables declaration//GEN-END:variables

}
