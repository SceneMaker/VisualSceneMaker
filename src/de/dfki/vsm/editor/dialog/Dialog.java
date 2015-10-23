package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.ios.ResourceLoader;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.WindowConstants;

/**
 * @author Not me
 */
public abstract class Dialog extends JDialog {
    protected final JPanel    mMainPanel = new JPanel(null);
    protected final ImageIcon mIcon      = ResourceLoader.loadImageIcon("/res/img/logo.png");
    protected final Font      mFont      = new Font("SansSerif", Font.PLAIN, 11);

    //
    protected Button mPressedButton = Button.CANCEL;

    //

    public enum Button { OK, CANCEL }

    //
    public Dialog(JDialog parent, String title, boolean modal) {
        super(parent, title, modal);
//        mMainPanel.setLayout(new BoxLayout(mMainPanel, BoxLayout.Y_AXIS));
    }

    public Dialog(JFrame parent, String title, boolean modal) {
        super(parent, title, modal);
//        mMainPanel.setLayout(new BoxLayout(mMainPanel, BoxLayout.Y_AXIS));
    }

    protected void packComponents() {

        // Register key listener for ESCAPE
        getRootPane().registerKeyboardAction(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                cancelActionPerformed();
            }
        }, KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0, true), JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);

        // Register key listener for ENTER
        getRootPane().registerKeyboardAction(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                okActionPerformed();
            }
        }, KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0, true), JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);

        // Pack components
        mMainPanel.setBackground(Color.white);
        setBackground(Color.white);
        add(mMainPanel);
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setIconImage(mIcon.getImage());
        setResizable(false);
        pack();
        setLocation(getParent().getX() + (getParent().getWidth() - getWidth()) / 2,
                    getParent().getY() + (getParent().getHeight() - getHeight()) / 2);
    }

    protected void packComponents(int width, int height) {

        // Register key listener for ESCAPE
        getRootPane().registerKeyboardAction(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                cancelActionPerformed();
            }
        }, KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0, true), JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);

        // Register key listener for ENTER
        getRootPane().registerKeyboardAction(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                okActionPerformed();
            }
        }, KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0, true), JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);

        // Pack components
        mMainPanel.setBackground(Color.white);
        add(mMainPanel);
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setIconImage(mIcon.getImage());
        setResizable(false);
        pack();
        setSize(new Dimension(width + getInsets().left + getInsets().right,
                              height + getInsets().top + getInsets().bottom+20));
        setLocation(getParent().getX() + (getParent().getWidth() - getWidth()) / 2,
                    getParent().getY() + (getParent().getHeight() - getHeight()) / 2);
    }

    protected void addComponent(JComponent comp, int x, int y, int width, int height) {
        comp.setBounds(x, y, width, height);
        mMainPanel.add(comp);
    }

    protected void addComponent(JComponent comp, int width, int height) {
        comp.setMinimumSize(new Dimension(width, height));
        comp.setPreferredSize(new Dimension(width, height));
        comp.setMaximumSize(new Dimension(width, height));
        mMainPanel.add(comp);
    }
    protected void addComponent(JComponent comp){
        mMainPanel.add(comp);
    }

    public Button getPressedButton() {
        return mPressedButton;
    }

    public JPanel getMainPanel() {
        return mMainPanel;
    }

    protected void dispose(Button button) {
        //System.err.println("disposing on button " + mPressedButton);
        mPressedButton = button;
        //System.err.println("disposing on button " + mPressedButton);
        setVisible(false);
        dispose();
    }

    public static Box.Filler getFillerBox(int xSize, int ySize, int maxXSize, int maxYSize) {
        return new Box.Filler(new Dimension(xSize, ySize), new Dimension(xSize, ySize),
                              new Dimension(maxXSize, maxYSize));
    }

    protected abstract void okActionPerformed();

    protected abstract void cancelActionPerformed();
}
