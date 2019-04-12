
/*
* To change this license header, choose License Headers in Project Properties.
* To change this template file, choose Tools | Templates
* and open the template in the editor.
 */
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
public class EditButton extends JLabel {
    private final Dimension buttonSize = new Dimension(20, 20);
    
    public EditButton() {
        setHorizontalAlignment(SwingConstants.RIGHT);
        setOpaque(false);
        setBackground(Color.white);
        setIcon(PreferencesDesktop.ICON_EDIT_STANDARD);
        setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
        setToolTipText("Edit");
        setIconTextGap(10);
        setFont(new Font("Helvetica", Font.PLAIN, 24));
        setFocusable(false);
        setPreferredSize(buttonSize);
        setMinimumSize(buttonSize);
        addMouseListener(new java.awt.event.MouseAdapter() {

//          public void mouseClicked(java.awt.event.MouseEvent evt) {
//              //savePreferences(true);
//          }
            public void mouseEntered(MouseEvent me) {
                setIcon(PreferencesDesktop.ICON_EDIT_ROLLOVER);
            }
            public void mouseExited(MouseEvent me) {
                setIcon(PreferencesDesktop.ICON_EDIT_STANDARD);
            }
        });
    }
}
