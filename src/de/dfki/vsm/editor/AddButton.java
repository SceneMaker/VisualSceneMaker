/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.editor;

import de.dfki.vsm.util.ios.ResourceLoader;
import java.awt.Color;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.MouseEvent;
import javax.swing.JLabel;
import javax.swing.SwingConstants;

/**
 *
 * @author mfallas
 */
public class AddButton extends JLabel {
    private final Dimension buttonSize = new Dimension(20, 20);
    
    public AddButton() {
        setHorizontalAlignment(SwingConstants.RIGHT);
        setOpaque(false);
        setBackground(Color.white);
        setIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/add.png"));
        setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
        setToolTipText("Add");
        setIconTextGap(10);
        setFont(new Font("Helvetica", Font.PLAIN, 24));
        setFocusable(false);
        setPreferredSize(buttonSize);
        setMinimumSize(buttonSize);
        addMouseListener(new java.awt.event.MouseAdapter() {

//            public void mouseClicked(java.awt.event.MouseEvent evt) {
//                //savePreferences(true);
//            }

            public void mouseEntered(MouseEvent me) {
                setIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/add_blue.png"));
            }

            public void mouseExited(MouseEvent me) {
                setIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/add.png"));

            }
        });

    }
}
