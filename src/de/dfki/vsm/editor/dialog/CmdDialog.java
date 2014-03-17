package de.dfki.vsm.editor.dialog;

import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.model.sceneflow.command.Command;
import de.dfki.vsm.sfsl.parser._SFSLParser_;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JTextField;

/**
 *
 * @author Gregor Mehlmann
 */
public class CmdDialog extends Dialog {

    // The command  that has to be maintained
    private Command mCommand;
    // GUI-Components
    private JTextField mInputTextField;
    private JButton mOkButton;
    private JButton mCancelButton;

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
        mOkButton = new JButton("Ok");
        mOkButton.setBounds(10, 35, 90, 20);
        mOkButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                okActionPerformed();
            }
        });
        mCancelButton = new JButton("Cancel");
        mCancelButton.setBounds(100, 35, 90, 20);
        mCancelButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                cancelActionPerformed();
            }
        });
        addCompoment(mInputTextField, 10, 10, 300, 20);
        addCompoment(mOkButton, 10, 35, 90, 20);
        addCompoment(mCancelButton, 100, 35, 90, 20);
        packComponents(320, 60);
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
            if (cmd != null && !_SFSLParser_.errorFlag) {
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
