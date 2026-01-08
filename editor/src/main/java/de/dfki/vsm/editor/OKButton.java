package de.dfki.vsm.editor;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.PreferencesDesktop;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseEvent;

//~--- JDK imports ------------------------------------------------------------

/**
 *
 * @author mfallas
 */
public class OKButton extends JLabel {
    private final Dimension buttonSize = new Dimension(135, 30);
   
    
    public OKButton() {
        setText("OK");
        setHorizontalAlignment(SwingConstants.RIGHT);
        setOpaque(true);
        setBackground(Color.white);
        setIcon(PreferencesDesktop.ICON_OK_STANDARD);
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
            @Override
            public void mouseEntered(MouseEvent me) {
                if (isEnabled())
                {
                    setIcon(PreferencesDesktop.ICON_OK_ROLLOVER);
                    setBackground(new Color(82, 127, 255));
                }
            }
            @Override
            public void mouseExited(MouseEvent me) {
                setIcon(PreferencesDesktop.ICON_OK_STANDARD);
                setBackground(new Color(255, 255, 255));
            }
        });
    }
}
