package de.dfki.vsm.editor.dialog;

import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.sfsl.parser._SFSLParser_;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JTextField;

/**
 *
 * @author Gregor Mehlmann
 */
public class CreateExpDialog extends Dialog {

    //
    private Expression mExpression;
    //
    private JTextField mInputTextField;
    private JButton mOkButton;
    private JButton mCancelButton;

    public CreateExpDialog(Expression expression) {
        super(Editor.getInstance(), "Specify Command", true);
        mExpression = expression;
        initComponents();
    }

    private void initComponents() {
        mInputTextField = new JTextField();
        mInputTextField.setBounds(10, 10, 300, 20);
        mOkButton = new JButton("Ok");
        mOkButton.setBounds(10, 35, 90, 20);
        mOkButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                mPressedButton = Button.OK;
                if (process()) {
                    dispose();
                }
            }
        });
        mCancelButton = new JButton("Cancel");
        mCancelButton.setBounds(100, 35, 90, 20);
        mCancelButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                mPressedButton = Button.CANCEL;
                dispose();
            }
        });
        addCompoment(mInputTextField, 10, 10, 300, 20);
        addCompoment(mOkButton, 10, 35, 90, 20);
        addCompoment(mCancelButton, 100, 35, 90, 20);
        packComponents(320, 60);
    }

    public Expression run() {
        setVisible(true);
        if (mPressedButton == Button.OK) {
            return mExpression;
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
            _SFSLParser_.parseResultType = _SFSLParser_.EXP;
            _SFSLParser_.run(inputString);
            Expression exp = _SFSLParser_.expResult;
            if (exp != null && !_SFSLParser_.errorFlag) {
                mExpression = exp;
                return true;
            } else {
                return false;
            }
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }
}
