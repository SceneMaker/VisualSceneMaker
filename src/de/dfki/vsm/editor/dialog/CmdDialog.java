package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.model.sceneflow.command.Command;
import de.dfki.vsm.sfsl.parser._SFSLParser_;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

//~--- JDK imports ------------------------------------------------------------

import javax.swing.JTextField;

/**
 *
 * @author Gregor Mehlmann
 */
public class CmdDialog extends Dialog {

    // The command  that has to be maintained
    private Command mCommand;

    // GUI-Components
    private JTextField   mInputTextField;
    private OKButton     mOkButton;
    private CancelButton mCancelButton;

    public CmdDialog(Command command) {
        super(Editor.getInstance(), "Specify Command", true);
        mCommand = command;
        initComponents();

        if (mCommand != null) {
            mInputTextField.setText(mCommand.getConcreteSyntax());
        }
    }

    private void initComponents() {
        mInputTextField = new JTextField();
        mInputTextField.setBounds(10, 10, 300, 20);
        mOkButton = new OKButton();
        mOkButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                okActionPerformed();
            }
        });
        mCancelButton = new CancelButton();
        mCancelButton.setBounds(100, 35, 90, 20);
        mCancelButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                cancelActionPerformed();
            }
        });
        // Button panel
        JPanel mButtonPanel = new JPanel();
        mButtonPanel.setLayout(new BoxLayout(mButtonPanel, BoxLayout.X_AXIS));
        mButtonPanel.add(Box.createHorizontalGlue());
        mButtonPanel.add(mCancelButton);
        mButtonPanel.add(Box.createHorizontalStrut(30));
        mButtonPanel.add(mOkButton);
        mButtonPanel.add(Box.createHorizontalStrut(10));
        
        Box finalBox = Box.createVerticalBox();
        finalBox.add(mInputTextField);
        finalBox.add(Box.createVerticalStrut(30));
        finalBox.add(mButtonPanel);
        
        addComponent(finalBox, 10, 10, 300, 130);
        packComponents(320, 150);
    }

    public Command run() {
        setVisible(true);

        if (mPressedButton == Button.OK) {
            return mCommand;
        } else {
            return null;
        }
    }

    @Override
    protected void okActionPerformed() {
        if (process()) {
            dispose(Button.OK);
        }
    }

    @Override
    protected void cancelActionPerformed() {
        dispose(Button.CANCEL);
    }

    private boolean process() {
        String inputString = mInputTextField.getText().trim();

        try {
            _SFSLParser_.parseResultType = _SFSLParser_.CMD;
            _SFSLParser_.run(inputString);

            Command cmd = _SFSLParser_.cmdResult;

            if ((cmd != null) &&!_SFSLParser_.errorFlag) {
                mCommand = cmd;

                return true;
            } else {
                return false;
            }
        } catch (Exception e) {
            return false;
        }
    }
}
