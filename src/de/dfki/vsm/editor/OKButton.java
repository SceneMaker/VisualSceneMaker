package de.dfki.vsm.editor;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.ios.ResourceLoader;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Color;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.MouseEvent;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.SwingConstants;

/**
 *
 * @author mfallas
 */
public class OKButton extends JLabel {
    private final Dimension buttonSize = new Dimension(135, 30);
    //Icons
    private final ImageIcon ICON_OK_STANDARD = ResourceLoader.loadImageIcon("/res/img/ok_icon_gray.png");
    private final ImageIcon ICON_OK_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/ok_icon_blue.png");
    
    public OKButton() {
        setText("OK");
        setHorizontalAlignment(SwingConstants.RIGHT);
        setOpaque(true);
        setBackground(Color.white);
        setIcon(ICON_OK_STANDARD);
        setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
        setToolTipText("OK");
        setIconTextGap(20);
        setFont(new Font("Helvetica", Font.PLAIN, 20));
        setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY, 1));
        setPreferredSize(buttonSize);
        setMinimumSize(buttonSize);
        addMouseListener(new java.awt.event.MouseAdapter() {

//          public void mouseClicked(java.awt.event.MouseEvent evt) {
//              //savePreferences(true);
//          }
            public void mouseEntered(MouseEvent me) {
                setIcon(ICON_OK_ROLLOVER);
                setBackground(new Color(82, 127, 255));
            }
            public void mouseExited(MouseEvent me) {
                setIcon(ICON_OK_STANDARD);
                setBackground(new Color(255, 255, 255));
            }
        });
    }
}
