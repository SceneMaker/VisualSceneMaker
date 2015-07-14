package de.dfki.vsm.editor.project.auxiliary;

import de.dfki.vsm.util.ios.ResourceLoader;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JToolBar;

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 */
public final class AuxiliaryToolBar extends JToolBar {

    // The pricked pin flag
    private boolean pinPricked = false;
    // The pin toolbar button 
    private JButton mPinButton;

    public AuxiliaryToolBar() {
        // Initialize the tool bar
        super("AuxiliaryToolBar", JToolBar.HORIZONTAL);
        // Initialize tool bar features
        setFloatable(false);
        setRollover(true);
        // Set an empty tool bar border
        setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0));
        // Initialize the GUI components
        initComponents();
    }

    // Get the pin pricked flag
    public final boolean isPinPricked() {
        return pinPricked;
    }

    // Set the pin pricked flag
    public final void prickPin() {
        // TODO: Make resource loading once at a global place
        mPinButton.setIcon(ResourceLoader.loadImageIcon("/res/img/pin_blue.png"));
        pinPricked = true;
    }

    private void initComponents() {
        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
        setOpaque(false);
        add(Box.createHorizontalStrut(2));
        //Create the pin button
        mPinButton = new JButton(ResourceLoader.loadImageIcon("/res/img/pin.png"));
        mPinButton.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/pin_blue.png"));
        mPinButton.setContentAreaFilled(false);
        mPinButton.setMargin(new Insets(20, 10, 20, 10));
        mPinButton.setFocusable(false);
        mPinButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (!pinPricked) {
                    mPinButton.setIcon(ResourceLoader.loadImageIcon("/res/img/pin_blue.png"));
                    pinPricked = true;
                } else {
                    mPinButton.setIcon(ResourceLoader.loadImageIcon("/res/img/pin.png"));
                    pinPricked = false;
                }
            }
        });
//        sanitizeTinyButton(mGesticonButton);
        sanitizeTinyButton(mPinButton);

        add(Box.createHorizontalGlue());
        add(mPinButton);
        mPinButton.setBounds(TOP, TOP, TOP, TOP);

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
