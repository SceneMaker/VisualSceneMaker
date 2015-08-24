package de.dfki.vsm.editor.project.auxiliary;

import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.model.project.EditorConfig;
import de.dfki.vsm.util.ios.ResourceLoader;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JToolBar;

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 */
public final class AuxiliaryToolBar extends JToolBar {

    // The pricked pin flag
    private boolean pinPricked;
    // The pin toolbar button 
    private JButton mPinButton;
    // The current editor project
    private final EditorProject mProject;
    // Editor configurations
    private final EditorConfig mEditorConfig;
    //ICONS
    private final ImageIcon ICON_PIN_STANDARD = ResourceLoader.loadImageIcon("/res/img/pin.png");
    private final ImageIcon ICON_PIN_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/pin_blue.png");
    
    public AuxiliaryToolBar(final EditorProject project) {
        // Initialize the tool bar
        super("AuxiliaryToolBar", JToolBar.HORIZONTAL);
        // Initialize the editor project
        mProject = project;
        //load project preferences 
        mEditorConfig = mProject.getEditorConfig();
        //load pin status 
        pinPricked = !Boolean.valueOf(mEditorConfig.getProperty("autohidebottombar"));
        
        // Initialize tool bar features
        setFloatable(false);
        setRollover(true);
        // Set an empty tool bar border
        setBorder(BorderFactory.createEmptyBorder());
        // Initialize the GUI components
        initComponents();
    }

    // Get the pin pricked flag
    public final boolean isPinPricked() {
        return pinPricked;
    }

    // Set the pin pricked flag
    public final void setPin(boolean state) {
        pinPricked = state;
        mPinButton.setIcon(pinPricked? ICON_PIN_ROLLOVER: ICON_PIN_STANDARD);
        mPinButton.setRolloverIcon(pinPricked? ICON_PIN_STANDARD : ICON_PIN_ROLLOVER);
        mEditorConfig.setProperty("autohidebottombar", String.valueOf(!pinPricked));
    }
    
    private void initComponents() {
        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
        setOpaque(false);
        add(Box.createHorizontalStrut(2));
        //Create the pin button
        mPinButton = new JButton();
        setPin(pinPricked);
        mPinButton.setContentAreaFilled(false);
        mPinButton.setMargin(new Insets(20, 10, 20, 10));
        mPinButton.setFocusable(false);
        mPinButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setPin(!pinPricked);
            }
        });
//        sanitizeTinyButton(mGesticonButton);
        sanitizeTinyButton(mPinButton);

        add(Box.createHorizontalGlue());
        add(mPinButton);
        //mPinButton.setBounds(TOP, TOP, TOP, TOP);

    }

    private void sanitizeTinyButton(JButton b) {
        Dimension bDim = new Dimension(30, 30);

        b.setMinimumSize(bDim);
        b.setMaximumSize(bDim);
        b.setPreferredSize(bDim);
        b.setOpaque(false);

//      b.setContentAreaFilled(false);
//      b.setFocusable(false);
        b.setBorder(BorderFactory.createEmptyBorder());
    }

}
