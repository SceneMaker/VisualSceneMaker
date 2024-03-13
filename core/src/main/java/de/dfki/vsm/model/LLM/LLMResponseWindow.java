package de.dfki.vsm.model.LLM;

import javax.swing.*;
import java.awt.*;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class LLMResponseWindow extends JFrame {

    public LLMResponseWindow(String initialText) {
        setTitle("LLM Response");
        JFrame newFrame = new JFrame("LLM Response");

        // Create a panel for the new window
        JPanel newPanel = new JPanel(new BorderLayout());
        newFrame.getContentPane().add(newPanel);
        JTextArea textArea = new JTextArea(initialText);

        // Create a text field for user input
        newPanel.add(new JScrollPane(textArea), BorderLayout.WEST);


        // Create a button to copy the text
        JButton copyButton = new JButton("Copy");
        newPanel.add(copyButton, BorderLayout.EAST);

        // Add action listener to the button
        copyButton.addActionListener(e -> copyText(textArea.getText()));

        newFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        newFrame.pack();
        newFrame.setLocationRelativeTo(this);
        newFrame.setVisible(true);
    }

    private void copyText(String textToCopy) {
        Toolkit.getDefaultToolkit().getSystemClipboard().setContents(
                new StringSelection(textToCopy), null);
    }

}