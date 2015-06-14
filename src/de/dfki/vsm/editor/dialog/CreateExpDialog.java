package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.sfsl.parser._SFSLParser_;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Color;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import javax.swing.JTextField;

/**
 *
 * @author Gregor Mehlmann
 */
public class CreateExpDialog extends Dialog {

    //
    private Expression mExpression;

    //
    private JTextField   mInputTextField;
    private OKButton     mOkButton;
    private CancelButton mCancelButton;

    public CreateExpDialog(Expression expression) {
        super(Editor.getInstance(), "Specify Command", true);
        mExpression = expression;
        initComponents();
    }

    private void initComponents() {
        mInputTextField = new JTextField();
        mInputTextField.setBounds(10, 10, 300, 20);
        mOkButton = new OKButton();
        mOkButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                mPressedButton = Button.OK;

                if (process()) {
                    dispose();
                }
            }
        });
        mCancelButton = new CancelButton();
        mCancelButton.setBounds(100, 35, 90, 20);
        mCancelButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                mPressedButton = Button.CANCEL;
                dispose();
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

            if ((exp != null) &&!_SFSLParser_.errorFlag) {
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
