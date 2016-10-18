package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.editor.util.HintTextField;
import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.model.sceneflow.ChartParser;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Color;
import java.awt.Dimension;
import java.awt.KeyEventDispatcher;
import java.awt.KeyboardFocusManager;
import java.awt.event.KeyEvent;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;


/**
 *
 * @author Gregor Mehlmann
 */
public class CreateExpDialog extends Dialog {

    //
    private Expression mExpression;

    //
    private HintTextField   mInputTextField;
    private OKButton     mOkButton;
    private CancelButton mCancelButton;
    private JLabel errorMsg;

    public CreateExpDialog(Expression expression) {
        super(EditorInstance.getInstance(), "Specify Command", true);
        mExpression = expression;
        initComponents();
    }

    private void initComponents() {
        mInputTextField = new HintTextField("Enter Expression");
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
        //Error message
        errorMsg = new JLabel("Information Required");
        errorMsg.setForeground(Color.white);
        errorMsg.setMinimumSize(new Dimension(300, 30));
        
        //Key listener need to gain focus on the text field
        KeyboardFocusManager.getCurrentKeyboardFocusManager().addKeyEventDispatcher(new KeyEventDispatcher() {

            @Override
            public boolean dispatchKeyEvent(KeyEvent ke) {
                //boolean keyHandled = false;
                if (ke.getID() == KeyEvent.KEY_PRESSED) {
                    if(!mInputTextField.hasFocus())
                    {
                        mInputTextField.setText(mInputTextField.getText()+ke.getKeyChar());
                        mInputTextField.requestFocus();
                    }
                }
                return false;
            }
        });
        
        Box finalBox = Box.createVerticalBox();
        finalBox.add(mInputTextField);
        finalBox.add(Box.createVerticalStrut(30));
        finalBox.add(errorMsg);
        finalBox.add(Box.createVerticalStrut(10));
        finalBox.add(mButtonPanel);
        
        addComponent(finalBox, 10, 10, 300, 160);
        packComponents(320, 180);
        mOkButton.requestFocus();
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
        if(mInputTextField.getText().length() == 0){
            mInputTextField.setBorder(BorderFactory.createLineBorder(Color.red));
            errorMsg.setForeground(Color.red);

            return false;
        }
        String inputString = mInputTextField.getText().trim();

        try {
            ChartParser.parseResultType = ChartParser.EXP;
            ChartParser.run(inputString);

            Expression exp = ChartParser.expResult;

            if ((exp != null) &&!ChartParser.errorFlag) {
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
