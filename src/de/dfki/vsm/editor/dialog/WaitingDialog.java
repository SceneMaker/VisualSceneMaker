package de.dfki.vsm.editor.dialog;

import de.dfki.vsm.util.tts.MaryTTsProcess;

import javax.swing.*;
import java.util.Observable;
import java.util.Observer;

/**
 * Created by alvaro on 5/28/16.
 */
public class WaitingDialog extends JDialog implements Observer{
    private JLabel messageLabel = new JLabel("Loading...");
    private JPanel messagePane = new JPanel();
    private String messageText;
    public WaitingDialog(){
        super();
        messageText= "" ;
        initComponents();
    }
    public WaitingDialog(String text){
        super();
        messageText = text;
        messageLabel.setText(text);
        initComponents();
    }

    public void setMessageText(String text){
        messageText = text;
        messageLabel.setText(text);
    }

    private void initComponents(){
        messagePane.add(messageLabel);
        add(messagePane);
        setTitle("Info");
        pack();
        setLocationRelativeTo(null);
    }


    @Override
    public void update(Observable o, Object arg) {
        if(o instanceof MaryTTsProcess){
            String message = (String) arg;
            if(message.equals("Server started") || message.equals("Server already running")){
                dispose();
            }else{
                setMessageText(message);
                pack();
                repaint();
                revalidate();
            }
        }
    }
}
