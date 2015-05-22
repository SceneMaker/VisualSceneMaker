package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.model.sceneflow.TEdge;
import de.dfki.vsm.runtime.dialogact.DialogActInterface;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Dimension;
import java.awt.event.KeyEvent;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

/**
 * @author Sergio Soto
 */
public class DialogActAttributes extends Dialog {

    // GUI-Components
    private JPanel       mAttributePanel;
    private JPanel       mValuesPanel;
    private JLabel       mAttributeLabel;
    private ButtonGroup  mDifficulyGroup;
    private JPanel       mButtonPanel;
    private OKButton     mOkButton;
    private CancelButton mCancelButton;
    private JPanel       mNamePanel;
    private JLabel       mNameLabel;
    private JTextField   mNameText;
    DialogActInterface   mDialogAct;
    String               mName;

    public DialogActAttributes(DialogActInterface dialogAct, String name) {
        super(Editor.getInstance(), "Dialog Act Attributes", true);
        mDialogAct = dialogAct;
        mName      = name;

        // Set the edge data
        // Init GUI-Components
        initComponents();
    }

    private void initComponents() {
        mNameLabel = new JLabel("Name: ");
        mNameText  = new JTextField(10);
        mNameText.setMaximumSize(new Dimension(325, 30));
        mNameText.setPreferredSize(new Dimension(325, 30));
        mNameText.setMinimumSize(new Dimension(325, 30));
        mNameText.setText(mName);

//      mNamePanel = new JPanel();
//      mNamePanel.setOpaque(false);
//      mNamePanel.setLayout(new BoxLayout(mNamePanel, BoxLayout.X_AXIS));
//      
//      mNamePanel.add(mNameLabel);
//      mNamePanel.add(mNameText);
//              
//      mMainPanel.setLayout(new BoxLayout(mMainPanel, BoxLayout.Y_AXIS));
//      mMainPanel.add(Box.createRigidArea(new Dimension(15, 10)));
//      
//      mMainPanel.add(mNamePanel);
        int offset  = 45;
        int initial = 20;

        for (String attribute : mDialogAct.getNLGAttributes()) {
            createAttributePanel(attribute);
            addComponent(mAttributePanel, 10, initial + offset, 390, 30);
            initial = initial + offset;

//          mMainPanel.add(mAttributePanel);
        }

        int buttonPos   = (initial > 220)
                          ? initial + 30
                          : 240;
        int finalHeight = (initial > 220)
                          ? Math.abs(300 - 220) + 300
                          : 300;

        // Init button panel
        initButtonPanel();
        addComponent(mNameLabel, 10, 10, 100, 30);
        addComponent(mNameText, 110, 10, 260, 30);
        addComponent(mCancelButton, 75, buttonPos, 125, 30);
        addComponent(mOkButton, 225, buttonPos, 125, 30);

//      addComponent(mButtonPanel, 200, 100);
//      mMainPanel.add(mButtonPanel);
//      mMainPanel.add(Box.createRigidArea(new Dimension(15, 10)));
        packComponents(400, finalHeight);
    }

    private void createAttributePanel(String attribute) {
        mAttributePanel = new JPanel();
        mAttributePanel.setOpaque(false);
        mAttributePanel.setLayout(new BoxLayout(mAttributePanel, BoxLayout.X_AXIS));
        mAttributeLabel = new JLabel(attribute);
        mAttributePanel.add(Box.createRigidArea(new Dimension(5, 0)));
        mAttributePanel.add(mAttributeLabel);
        mAttributePanel.add(Box.createHorizontalGlue());
        mDifficulyGroup = new ButtonGroup();

        for (String value : mDialogAct.getNLGAttributeValues(attribute)) {
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
        mCancelButton = new CancelButton();
        mCancelButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                cancelActionPerformed();
            }
        });

        // Ok button
        mOkButton = new OKButton();
        mOkButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                okActionPerformed();
            }
        });

        // Button panel
        mButtonPanel = new JPanel(null);
        mButtonPanel.setOpaque(false);
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
