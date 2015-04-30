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
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.SwingConstants;

/**
 *
 * @author mfallas
 */
public class OKButton extends JLabel {
    private final Dimension buttonSize = new Dimension(125, 30);
    
    public OKButton() {

        setText("OK");
        setHorizontalAlignment(SwingConstants.RIGHT);
        setOpaque(true);
        setBackground(Color.white);
        setIcon(ResourceLoader.loadImageIcon("/res/img/ok_icon_gray.png"));
        setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
        setToolTipText("OK");
        setIconTextGap(20);
        setFont(new Font("Helvetica", Font.PLAIN, 20));
        setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY, 1));
        setPreferredSize(buttonSize);
        setMinimumSize(buttonSize);
        addMouseListener(new java.awt.event.MouseAdapter() {

//            public void mouseClicked(java.awt.event.MouseEvent evt) {
//                //savePreferences(true);
//            }

            public void mouseEntered(MouseEvent me) {
                setIcon(ResourceLoader.loadImageIcon("/res/img/ok_icon_blue.png"));
                setBackground(new Color(82, 127, 255));
            }

            public void mouseExited(MouseEvent me) {
                setIcon(ResourceLoader.loadImageIcon("/res/img/ok_icon_gray.png"));
                setBackground(new Color(255, 255, 255));

            }
        });

    }
}
