
/*
* To change this template, choose Tools | Templates
* and open the template in the editor.
 */
package de.dfki.vsm.editor;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.Preferences;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Color;
import java.awt.Font;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.UIManager;

/**
 *
 * @author Patrick
 */
public class EdgeTypeSelection extends JPanel {
    Font f = UIManager.getDefaults().getFont("Tree.font");

    EdgeTypeSelection() {
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        setBackground(Color.WHITE);
        setBorder(BorderFactory.createEtchedBorder());

        JLabel headline = new JLabel("New Edge Type");

        headline.setFont(f);
        add(headline);
        initEdgeSelectors();
    }

    private void initEdgeSelectors() {
        JRadioButton rb;
        ButtonGroup  bg = new ButtonGroup();

        for (Edge.TYPE e : Edge.TYPE.values()) {
            JPanel p = new JPanel();

            p.setBackground(Color.WHITE);
            p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));

            switch (e) {
            case EEDGE :
                rb = new JRadioButton("Free Transition", Preferences.ICON_RADIOBUTTON_UNSELECTED);
                rb.setSelectedIcon(Preferences.ICON_RADIOBUTTON_SELECTED);
                rb.setFont(f);
                rb.setBackground(Color.WHITE);
                p.add(rb);
                bg.add(rb);

                break;

            case FEDGE :
                rb = new JRadioButton("Forking Transition", Preferences.ICON_RADIOBUTTON_UNSELECTED);
                rb.setSelectedIcon(Preferences.ICON_RADIOBUTTON_SELECTED);
                rb.setFont(f);
                rb.setBackground(Color.WHITE);
                rb.setForeground(new Color(82, 51, 161));
                p.add(rb);
                bg.add(rb);
                p.add(rb);

                break;

            case TEDGE :
                rb = new JRadioButton("Timeout Transition", Preferences.ICON_RADIOBUTTON_UNSELECTED);
                rb.setSelectedIcon(Preferences.ICON_RADIOBUTTON_SELECTED);
                rb.setFont(f);
                rb.setBackground(Color.WHITE);
                p.add(rb);
                bg.add(rb);
                p.add(rb);

                break;

            case CEDGE :
                rb = new JRadioButton("Conditional Transition", Preferences.ICON_RADIOBUTTON_UNSELECTED);
                rb.setSelectedIcon(Preferences.ICON_RADIOBUTTON_SELECTED);
                rb.setFont(f);
                rb.setBackground(Color.WHITE);
                p.add(rb);
                bg.add(rb);
                p.add(rb);

                break;

            case PEDGE :
                rb = new JRadioButton("Probabilisitic Transition", Preferences.ICON_RADIOBUTTON_UNSELECTED);
                rb.setSelectedIcon(Preferences.ICON_RADIOBUTTON_SELECTED);
                rb.setFont(f);
                rb.setBackground(Color.WHITE);
                p.add(rb);
                bg.add(rb);
                p.add(rb);

                break;

            case IEDGE :
                rb = new JRadioButton("Interruptive Transition", Preferences.ICON_RADIOBUTTON_UNSELECTED);
                rb.setSelectedIcon(Preferences.ICON_RADIOBUTTON_SELECTED);
                rb.setFont(f);
                rb.setBackground(Color.WHITE);
                p.add(rb);
                bg.add(rb);
                p.add(rb);

                break;
            }

            add(p);
        }
    }
}
