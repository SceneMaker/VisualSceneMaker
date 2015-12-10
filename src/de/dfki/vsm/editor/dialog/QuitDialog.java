package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.OKButton;

import de.dfki.vsm.util.ios.ResourceLoader;

//~--- JDK imports ------------------------------------------------------------

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.MouseEvent;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
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
    //private static QuitDialog   sInstance            = null;
    private JPanel              mContentPanel        = null;
    private OKButton            mYesButton;
    private CancelButton        mNoButton;
    private CancelButton        mCancelButton;
    //private EditorInstance      mEditorInstance = EditorInstance.getInstance();
    private JLabel              mExitMessage;
    
    private int                 mFinalExitMessage; //TAKES ONE OF THE VALUES OF THE RETURN MESSAGES

    
    
    ///TYPES OF DIALOGS --- AFFECTS THE MESSAGE SHOWN
    public static final int     EXIT_DIALOG = 0;
    public static final int     CLOSE_PROJ_DIALOG = 1;
    //RETURN MESSAGES
    public static final int     SAVE_AND_EXIT = 0;
    public static final int     EXIT_AND_IGNORE = 1;
    public static final int     CANCEL_CLOSING = 2;
    //MESSAGES TO BE SHOWN 
    private String              exitMessage;
    private String              yesButtonMessage;
    private String              noButtonMessage;
    private String              cancelButtonMessage;
    //ICONS 
    private ImageIcon ICON_EXIT_STANDARD = ResourceLoader.loadImageIcon("/res/img/exit_icon.png");
    private ImageIcon ICON_EXIT_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/exit_icon_blue.png");
    
    // Construction
    public QuitDialog(int quitType) {
        super(EditorInstance.getInstance(), "Quit", false);
        EditorInstance.getInstance().addEscapeListener(this);
        if (quitType == EXIT_DIALOG)
        {
            exitMessage ="<html><body>This Project has been modified but not saved. <br> Do you want to save before quitting?</body></html>";
            yesButtonMessage = "Save";
            noButtonMessage = "Discard";
            cancelButtonMessage = "Cancel";
            this.setTitle("Save before exiting?");
        }
        else if(quitType == CLOSE_PROJ_DIALOG)
        {
            exitMessage ="<html><body>This Project has been modified but not saved. <br> Do you want to save before closing it?</body></html>";
            yesButtonMessage = "Save";
            noButtonMessage = "Close";
            cancelButtonMessage = "Cancel";
            this.setTitle("Save before closing?");
        }
        
        //CHANGE MODALITY 
        setModalityType(Dialog.ModalityType.DOCUMENT_MODAL);
        
        // the exit message
        mExitMessage = new JLabel(exitMessage);
        mExitMessage.setIcon(ResourceLoader.loadImageIcon("/res/img/warning_icon.png"));
        mExitMessage.setMinimumSize(new Dimension(450, 120));
        mExitMessage.setMaximumSize(new Dimension(450, 120));
        mExitMessage.setPreferredSize(new Dimension(450, 120));
        // Init the button
        // Ok button
        mYesButton = new OKButton();
        mYesButton.setText(yesButtonMessage);
        mYesButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                //mEditorInstance.save();
               // mEditorInstance.disposeAfterDialog();
                //mEditorInstance.saveMessage = true;
                mFinalExitMessage = SAVE_AND_EXIT;
                dispose();
            }
        });
        //NO BUTTON
        mNoButton = new CancelButton();
        mNoButton.setIcon(ICON_EXIT_STANDARD);
        mNoButton.setText(noButtonMessage);
        mNoButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                //mParentEditor.disposeAfterDialog();
                //mParentEditor.saveMessage = true;
                mFinalExitMessage = EXIT_AND_IGNORE;
                dispose();
            }
            public void mouseEntered(MouseEvent me) {
                mNoButton.setIcon(ICON_EXIT_ROLLOVER);
                mNoButton.setBackground(new Color(82, 127, 255));
            }
            public void mouseExited(MouseEvent me) {
                mNoButton.setIcon(ICON_EXIT_STANDARD);
                mNoButton.setBackground(new Color(255, 255, 255));
            }
        });
        mCancelButton = new CancelButton();
        mCancelButton.setText(cancelButtonMessage);
        mCancelButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                //mParentEditor.saveMessage = false;
                mFinalExitMessage = CANCEL_CLOSING;
                dispose();
            }
        });
        // Button Panel
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
    //RETURN EXIT MESSAGE
    public int getExitMessage() {
        return mFinalExitMessage;
    }
    // Get the singelton instance
    /*public static QuitDialog getInstance(ProjectEditor pE) {
        if (sInstance == null) {
            sInstance = new QuitDialog(pE);
        }

        return sInstance;
    }*/
}
