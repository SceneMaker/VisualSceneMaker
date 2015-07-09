package de.dfki.vsm.api.hcm;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Dimension;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JFrame;
import javax.swing.JPanel;

/**
 * @author Not me
 * @author Kathrin Janowski
 */
public final class HCMUserInterface extends JFrame implements ActionListener {

    // The Screen Size
    private final Dimension mScreenSize = Toolkit.getDefaultToolkit().getScreenSize();

    // The Window Size
    private final Dimension mWindowSize = mScreenSize;

    // The Window Location
    private final Point mWindowLocation = new Point(0, 0);

    // The Scene Player
    private final HCMScenePlayer mScenePlayer;

    // The Main Panel Board
    private final JPanel mContentPanel;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    // Construct User Interface
    public HCMUserInterface(final HCMScenePlayer player) {

        // Inittialize Super
        super("HCM User interface");

        // Initialize Player
        mScenePlayer = player;

        // Set Not Rezizable
        // setResizable(false);
        // Set Always On Top
        // setAlwaysOnTop(true);
        // Set Undecorated
        // setUndecorated(true);
        // Set Transparent
        // setBackground(new Color(0, 0, 0, 0));
        // Set Full Screen
        // setExtendedState(MAXIMIZED_BOTH);
        // Create the Center Panel
        mContentPanel = new JPanel();

        // Add The Content Panel
        setContentPane(mContentPanel);

        // Set The Frame Location
        setLocation(mWindowLocation);
        setMinimumSize(mWindowSize);

        // Finally Pack The Frame
        pack();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void actionPerformed(final ActionEvent event) {

        // final Object source = event.getSource();
    }
}
