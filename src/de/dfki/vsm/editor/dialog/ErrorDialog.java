package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.runtime.events.AbortionEvent;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.WindowConstants;

/**
 * @author Not me
 * @author Patrick Gebhard
 */
public class ErrorDialog extends JDialog {
    private static ErrorDialog sSingeltonInstance = null;
    private JPanel             mMainPanel;
    private JPanel             mErrorPanel;
    private JPanel             mButtonsPanel;
    private JList              mList;
    private JScrollPane        mScrollPane;
    private JButton            mOkButton;
    private JButton            mClearButton;
    private int                mNum;

    private ErrorDialog() {
        super(EditorInstance.getInstance(), "Error Console", false);
        EditorInstance.getInstance().addEscapeListener(this);
        initComponents();
    }

    public static ErrorDialog getInstance() {
        if (sSingeltonInstance == null) {
            sSingeltonInstance = new ErrorDialog();
        }

        return sSingeltonInstance;
    }

    public void addError(AbortionEvent event) {

        //
        // TODO: clear old error list
        //
        JTextArea textArea = new JTextArea();

        textArea.setLineWrap(true);
        textArea.setWrapStyleWord(true);
        textArea.setText(event.getMessage());
        textArea.setBorder(BorderFactory.createEtchedBorder());
        textArea.getRows();

        if (mNum % 2 == 0) {
            textArea.setBackground(Color.cyan);
        } else {
            textArea.setBackground(Color.cyan.darker());
        }

        ((DefaultListModel) mList.getModel()).addElement(textArea.getText());
        mNum++;
    }

    private void initComponents() {
        mButtonsPanel = new JPanel();
        mButtonsPanel.setLayout(new BoxLayout(mButtonsPanel, BoxLayout.X_AXIS));
        mOkButton = new JButton("Ok");
        mOkButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                dispose();
            }
        });
        mClearButton = new JButton("Clear");
        mClearButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                ((DefaultListModel) mList.getModel()).clear();
            }
        });
        mButtonsPanel.add(Box.createHorizontalGlue());
        mButtonsPanel.add(mOkButton);
        mButtonsPanel.add(mClearButton);
        mButtonsPanel.add(Box.createRigidArea(new Dimension(5, 0)));

        // Do the layout
        mErrorPanel = new JPanel();
        mErrorPanel.setLayout(new BoxLayout(mErrorPanel, BoxLayout.Y_AXIS));
        mList       = new JList(new DefaultListModel());
        mScrollPane = new JScrollPane(mList);
        mScrollPane.setPreferredSize(new Dimension(600, 300));
        mErrorPanel.add(mScrollPane);
        mMainPanel = new JPanel();
        mMainPanel.setLayout(new BoxLayout(mMainPanel, BoxLayout.Y_AXIS));
        mMainPanel.add(mErrorPanel);
        mMainPanel.add(mButtonsPanel);
        add(mMainPanel);
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setResizable(false);
        pack();
        setLocation(getParent().getLocation().x + (getParent().getWidth() - getWidth()) / 2,
                    getParent().getLocation().y + (getParent().getHeight() - getHeight()) / 2);
    }
}
