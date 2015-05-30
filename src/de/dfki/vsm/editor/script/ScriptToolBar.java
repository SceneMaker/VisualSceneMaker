package de.dfki.vsm.editor.script;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.AddButton;
import de.dfki.vsm.model.configs.ProjectPreferences;
import de.dfki.vsm.util.ios.ResourceLoader;

//~--- JDK imports ------------------------------------------------------------

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JToolBar;

/**
 * @author Patrick Gebhard
 */
public class ScriptToolBar extends JToolBar {
    private boolean            pinPricked = false;
    private JButton            mGesticonButton;
    private ScriptEditorPanel  mParent;
    private ProjectPreferences mPreferences;

    // Button to keep the script toolbar visible
    private JButton mPinScriptToolbar;

    public ScriptToolBar(ScriptEditorPanel parent) {
        super("Scenes Tool Bar", JToolBar.HORIZONTAL);
        mParent      = parent;
        mPreferences = mParent.getPreferences();
        setFloatable(false);
        setRollover(true);
        setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0));

        // setBorder(BorderFactory.createLineBorder(Color.yellow));
        initComponents();
    }

    /**
     * Function to know if the script editor panel is fixed or can be hidden
     *
     * @return
     */
    public boolean isPinPricked() {
        return pinPricked;
    }

    /**
     * Fixes the split pane
     */
    public void prickPin() {
        mPinScriptToolbar.setIcon(ResourceLoader.loadImageIcon("/res/img/pin_blue.png"));
        pinPricked = true;
    }

    private void initComponents() {
        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
        setOpaque(false);
        add(Box.createHorizontalStrut(2));
        mGesticonButton = add(new AbstractAction("ACTION_SHOW_ELEMENTS",
                Boolean.valueOf(mPreferences.getProperty("showsceneelements"))
                ? ResourceLoader.loadImageIcon("/res/img/toolbar_icons/more.png")
                : ResourceLoader.loadImageIcon("/res/img/toolbar_icons/less.png")) {
            public void actionPerformed(ActionEvent evt) {
                boolean state = mParent.showElementDisplay();

                changeGesticonDisplayButtonState(state);
                revalidate();
                repaint();
            }
        });
        mGesticonButton.setRolloverIcon(Boolean.valueOf(mPreferences.getProperty("showsceneelements"))
                                        ? ResourceLoader.loadImageIcon("/res/img/toolbar_icons/more_blue.png")
                                        : ResourceLoader.loadImageIcon("/res/img/toolbar_icons/less_blue.png"));
        mGesticonButton.setContentAreaFilled(false);
        mGesticonButton.setFocusable(false);

        //Create the pin button
        mPinScriptToolbar = new JButton(ResourceLoader.loadImageIcon("/res/img/pin.png"));
        mPinScriptToolbar.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/pin_blue.png"));
        mPinScriptToolbar.setMargin(new Insets(20, 10, 20, 10));
        mPinScriptToolbar.setFocusable(false);
        mPinScriptToolbar.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (!pinPricked) {
                    mPinScriptToolbar.setIcon(ResourceLoader.loadImageIcon("/res/img/pin_blue.png"));
                    pinPricked = true;
                } else {
                    mPinScriptToolbar.setIcon(ResourceLoader.loadImageIcon("/res/img/pin.png"));
                    pinPricked = false;
                }
            }
        });
        sanitizeTinyButton(mGesticonButton);
        sanitizeTinyButton(mPinScriptToolbar);
        add(mGesticonButton, BorderLayout.WEST);
        add(Box.createHorizontalGlue());
        add(mPinScriptToolbar);
        mPinScriptToolbar.setBounds(TOP, TOP, TOP, TOP);

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

    private void changeGesticonDisplayButtonState(boolean state) {
        if (state) {
            mGesticonButton.setIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/more.png"));
            mPreferences.setProperty("showsceneelements", "true");
            mPreferences.save(mParent.getPreferencesFileName());
        } else {
            mGesticonButton.setIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/less.png"));
            mPreferences.setProperty("showsceneelements", "false");
            mPreferences.save(mParent.getPreferencesFileName());
        }

        mGesticonButton.setRolloverIcon(Boolean.valueOf(mPreferences.getProperty("showsceneelements"))
                                        ? ResourceLoader.loadImageIcon("/res/img/toolbar_icons/more_blue.png")
                                        : ResourceLoader.loadImageIcon("/res/img/toolbar_icons/less_blue.png"));
    }
}
