package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.model.sceneflow.TEdge;
import de.dfki.vsm.runtime.dialogact.DialogActInterface;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

/**
 * @author Sergio Soto
 */
public class DialogActAttributes extends Dialog {

    // GUI-Components    
    private JPanel      mAttributePanel;
    private JPanel      mValuesPanel;
    private JLabel      mAttributeLabel;
    private ButtonGroup mDifficulyGroup;
    private JPanel      mButtonPanel;
    private JButton     mOkButton;
    private JButton     mCancelButton;
    private JPanel      mNamePanel;
    private JLabel      mNameLabel;
    private JTextField  mNameText;
  
    
    DialogActInterface mDialogAct;
    String mName;

    public DialogActAttributes(DialogActInterface dialogAct, String name) {
        super(Editor.getInstance(), "Dialog Act Attributes", true);

        mDialogAct = dialogAct;
        mName = name;
        // Set the edge data
        // Init GUI-Components
        initComponents();
    }

    private void initComponents() {
        
        mNameLabel = new JLabel("Name: ");
        mNameText  = new JTextField(10);
        mNameText.setText(mName);
        
        mNamePanel = new JPanel();
        mNamePanel.setLayout(new BoxLayout(mNamePanel, BoxLayout.X_AXIS));
        
        mNamePanel.add(mNameLabel);
        mNamePanel.add(mNameText);
                
        mMainPanel.setLayout(new BoxLayout(mMainPanel, BoxLayout.Y_AXIS));
        mMainPanel.add(Box.createRigidArea(new Dimension(15, 10)));
        
        mMainPanel.add(mNamePanel);
        
        for(String attribute: mDialogAct.getNLGAttributes()){
            createAttributePanel(attribute);
            mMainPanel.add(mAttributePanel);
        }

        // Init button panel
        initButtonPanel();
        
 
       
        addCompoment(mButtonPanel, 320, 100);
        mMainPanel.add(Box.createRigidArea(new Dimension(15, 10)));
        packComponents(330, 150);
    }

    

        
    private void createAttributePanel(String attribute) {
        
        mAttributePanel = new JPanel();
        mAttributePanel.setLayout(new BoxLayout(mAttributePanel, BoxLayout.X_AXIS));
               
        mAttributeLabel = new JLabel(attribute);
        mAttributePanel.add(Box.createRigidArea(new Dimension(5, 0)));
        mAttributePanel.add(mAttributeLabel);
        mAttributePanel. add(Box.createHorizontalGlue());
         
        mDifficulyGroup = new ButtonGroup();
        
        for(String value: mDialogAct.getNLGAttributeValues(attribute)){
            
            JRadioButton newButton = new JRadioButton(value);
            newButton.setMnemonic(KeyEvent.VK_D);
            newButton.setActionCommand(value);  
            mDifficulyGroup.add(newButton);
            mAttributePanel.add(Box.createRigidArea(new Dimension(5, 5)));
            mAttributePanel.add(newButton);
        }
        mAttributePanel.add(Box.createRigidArea(new Dimension(15, 5)));

    
    }
        
    private void initButtonPanel() {
        
        
        // Cancel button
        mCancelButton = new JButton("Cancel");
        mCancelButton.setBounds(210, 10, 100, 20);
        mCancelButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                cancelActionPerformed();
            }
        });

        // Ok button
        mOkButton = new JButton("Ok");
        mOkButton.setBounds(110, 10, 100, 20);
        mOkButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                okActionPerformed();
            }
        });

        

        // Button panel
        mButtonPanel = new JPanel(null);
        mButtonPanel.add(mCancelButton);
        mButtonPanel.add(mOkButton);
        
    }

    public TEdge run() {
        setVisible(true);

        if (mPressedButton == Dialog.Button.OK) {
            return null;
        } else {
            return null;
        }
    }

    @Override
    protected void okActionPerformed() {
        if (process()) {
            dispose(Dialog.Button.OK);
        }
    }

    @Override
    protected void cancelActionPerformed() {
        dispose(Dialog.Button.CANCEL);
    }

    private boolean process() {
        return false;
    }
}

