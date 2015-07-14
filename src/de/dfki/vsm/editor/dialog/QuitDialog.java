package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.editor.ProjectEditor;

import static de.dfki.vsm.editor.dialog.Dialog.getFillerBox;
import de.dfki.vsm.util.ios.ResourceLoader;

//~--- JDK imports ------------------------------------------------------------

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.PopupMenu;
import java.awt.Toolkit;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.Box;


import javax.swing.BoxLayout;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * @author Martín Fallas
 * This Dialog is open when closing a project or the system itself 
 * and one or more projects have not been saved
 */
public class QuitDialog extends JDialog {

    // Singelton instance
    private static QuitDialog   sInstance            = null;
    private JPanel              mContentPanel        = null;
    private OKButton            mYesButton;
    private CancelButton        mNoButton;
    private CancelButton        mCancelButton;
    private ProjectEditor       mParentEditor;
    private JLabel              mExitMessage;
    // Construction
    public QuitDialog(ProjectEditor parentEdit) {
        super(Editor.getInstance(), "Quit", false);
        mParentEditor = parentEdit;
        
        //CHANGE MODALITY 
        setModalityType(Dialog.ModalityType.DOCUMENT_MODAL);
        
        // the exit message
        mExitMessage = new JLabel("<html><body>This Project has been modified but not saved. <br> Do you want to save before quitting?</body></html>");
        mExitMessage.setIcon(ResourceLoader.loadImageIcon("/res/img/warning_icon.png"));
        mExitMessage.setMinimumSize(new Dimension(450, 120));
        mExitMessage.setMaximumSize(new Dimension(450, 120));
        mExitMessage.setPreferredSize(new Dimension(450, 120));
        // Init the button
        // Ok button
        mYesButton = new OKButton();
        mYesButton.setText("Save");
        mYesButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                mParentEditor.save();
                mParentEditor.disposeAfterDialog();
                mParentEditor.saveMessage = true;
                dispose();
            }
        });
        //NO BUTTON
        mNoButton = new CancelButton();
        mNoButton.setIcon(ResourceLoader.loadImageIcon("/res/img/exit_icon.png"));
        mNoButton.setText("Close");
        mNoButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                mParentEditor.disposeAfterDialog();
                mParentEditor.saveMessage = true;
                dispose();
            }
            public void mouseEntered(MouseEvent me) {
                mNoButton.setIcon(ResourceLoader.loadImageIcon("/res/img/exit_icon_blue.png"));
                mNoButton.setBackground(new Color(82, 127, 255));
            }
            public void mouseExited(MouseEvent me) {
                mNoButton.setIcon(ResourceLoader.loadImageIcon("/res/img/exit_icon.png"));
                mNoButton.setBackground(new Color(255, 255, 255));
            }
        });
        mCancelButton = new CancelButton();
        mCancelButton.setText("Cancel");
        mCancelButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                mParentEditor.saveMessage = false;
                dispose();
            }
        });

        JPanel mButtonPanel = new JPanel();
        mButtonPanel.setLayout(new BoxLayout(mButtonPanel, BoxLayout.X_AXIS));
        mButtonPanel.add(Box.createHorizontalGlue());
        mButtonPanel.add(mYesButton);
        mButtonPanel.add(Box.createHorizontalStrut(30));
        mButtonPanel.add(mNoButton);
        mButtonPanel.add(Box.createHorizontalStrut(30));
        mButtonPanel.add(mCancelButton);
        mButtonPanel.add(Box.createHorizontalStrut(10));

        
        mContentPanel = new JPanel();
        mContentPanel.setLayout(new BoxLayout(mContentPanel, BoxLayout.Y_AXIS));
        mContentPanel.add(Box.createVerticalStrut(20));
        mContentPanel.add(mExitMessage);
        mContentPanel.add(mButtonPanel);
        getContentPane().add(mContentPanel, BorderLayout.EAST);
        //setSize(new Dimension(600, 400));
        setSize(500, 230 );
        Dimension bounds  = Toolkit.getDefaultToolkit().getScreenSize();
        Dimension abounds = getSize(); 
        setLocation((bounds.width - abounds.width) / 2, (bounds.height - abounds.height) / 3);
        setResizable(false);
        setVisible(true);
    }
    // Get the singelton instance
    public static QuitDialog getInstance(ProjectEditor pE) {
        if (sInstance == null) {
            sInstance = new QuitDialog(pE);
        }

        return sInstance;
    }
}
