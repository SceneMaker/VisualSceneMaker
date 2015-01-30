package de.dfki.vsm.editor.script;

import de.dfki.vsm.model.configs.ProjectPreferences;
import de.dfki.vsm.util.ios.ResourceLoader;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JToolBar;

/**
 * @author Patrick Gebhard
 */
public class ScriptToolBar extends JToolBar {

    private JButton mGesticonButton;
    private ScriptEditorPanel mParent;
     private ProjectPreferences mPreferences;
    //Button to keep the script toolbar visible
    private JButton mPinScriptToolbar;
    private boolean pinPricked = false;

    /**
     * Function to know if the script editor panel is fixed or can be hidden
     * @return 
     */
    public boolean isPinPricked() {
        return pinPricked;
    }

    public ScriptToolBar(ScriptEditorPanel parent) {
        super("Scenes Tool Bar", JToolBar.HORIZONTAL);
        mParent = parent;
        mPreferences =  mParent.getPreferences();
        setFloatable(false);
        setRollover(true);
        
        setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0));
        //setBorder(BorderFactory.createLineBorder(Color.yellow));
        initComponents();        
    
    }

    private void initComponents() {
        setLayout(new BorderLayout());
        add(Box.createHorizontalStrut(2));
        mGesticonButton = add(new AbstractAction("ACTION_SHOW_ELEMENTS",
                Boolean.valueOf(mPreferences.getProperty("showsceneelements"))
                ? ResourceLoader.loadImageIcon("/res/img/new/less.png")
                : ResourceLoader.loadImageIcon("/res/img/new/more.png")) {

            public void actionPerformed(ActionEvent evt) {
                boolean state = mParent.showElementDisplay();
                changeGesticonDisplayButtonState(state);
                revalidate();
                repaint();
            }
        });
        //Create the pin button
        mPinScriptToolbar = new JButton(ResourceLoader.loadImageIcon("/res/img/pin.png"));
        mPinScriptToolbar.setFocusable(false);
        mPinScriptToolbar.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
               if(!pinPricked){
                   mPinScriptToolbar.setIcon(ResourceLoader.loadImageIcon("/res/img/prickedpin.png"));
                   pinPricked = true;
               }
               else{
                   mPinScriptToolbar.setIcon(ResourceLoader.loadImageIcon("/res/img/pin.png"));
                   pinPricked = false;
               }
            }
        });
        sanitizeTinyButton(mGesticonButton);
        sanitizeTinyButton(mPinScriptToolbar);
        add(mGesticonButton, BorderLayout.WEST);
        add(mPinScriptToolbar, BorderLayout.EAST);
        mPinScriptToolbar.setBounds(TOP, TOP, TOP, TOP);
        add(Box.createHorizontalGlue());
    }

    private void sanitizeTinyButton(JButton b) {
        Dimension bDim = new Dimension(22, 22);
        b.setMinimumSize(bDim);
        b.setMaximumSize(bDim);
        b.setPreferredSize(bDim);
        b.setOpaque(false);
        b.setBorder(BorderFactory.createEmptyBorder());
    }

    private void changeGesticonDisplayButtonState(boolean state) {
        if (state) {
            mGesticonButton.setIcon(ResourceLoader.loadImageIcon("/res/img/new/less.png"));
            mPreferences.setProperty("showsceneelements", "true");
            mPreferences.save(mParent.getPreferencesFileName());
 
        } else {
            mGesticonButton.setIcon(ResourceLoader.loadImageIcon("/res/img/new/more.png"));
            mPreferences.setProperty("showsceneelements", "false");
            mPreferences.save(mParent.getPreferencesFileName());
        }
    }
}
