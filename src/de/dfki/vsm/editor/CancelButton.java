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
import javax.swing.JLabel;
import javax.swing.SwingConstants;

/**
 *
 * @author mfallas
 */
public class CancelButton extends JLabel {
    private final Dimension buttonSize = new Dimension(135, 30);

    public CancelButton() {
        setText("Cancel");
        setHorizontalAlignment(SwingConstants.RIGHT);
        setOpaque(true);
        setBackground(Color.white);
        setIcon(ResourceLoader.loadImageIcon("/res/img/cancel_icon_gray.png"));
        setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
        setToolTipText("Cancel");
        setIconTextGap(10);
        setFont(new Font("Helvetica", Font.PLAIN, 20));
        setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY, 1));
        setPreferredSize(buttonSize);
        setMinimumSize(buttonSize);
        addMouseListener(new java.awt.event.MouseAdapter() {

//          public void mouseClicked(java.awt.event.MouseEvent evt) {
//              //savePreferences(true);
//          }
            public void mouseEntered(MouseEvent me) {
                setIcon(ResourceLoader.loadImageIcon("/res/img/cancel_icon_blue.png"));
                setBackground(new Color(82, 127, 255));
            }
            public void mouseExited(MouseEvent me) {
                setIcon(ResourceLoader.loadImageIcon("/res/img/cancel_icon_gray.png"));
                setBackground(new Color(255, 255, 255));
            }
        });
    }
}
