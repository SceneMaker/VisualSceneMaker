
/*
* To change this template, choose Tools | Templates
* and open the template in the editor.
 */
package de.dfki.vsm.editor.util;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.Comment;
import de.dfki.vsm.editor.Edge;
import de.dfki.vsm.editor.Node;
import de.dfki.vsm.editor.StartSign;
import de.dfki.vsm.editor.VarBadgeLocal;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.LayoutManager;

/**
 *
 * @author Patrick Gebhard
 */

/**
 * Use the LayoutManager facility to compute sizes
 */
public class SceneFlowLayoutManager implements LayoutManager {
    private Dimension mSize = new Dimension(0, 0);

    public void addLayoutComponent(String name, Component comp) {

        // System.out.println("adding component");
    }

    public void removeLayoutComponent(Component comp) {

        // System.out.println("removing component");
    }

    public Dimension preferredLayoutSize(Container parent) {

        // System.out.println("compute preferred size");
        recomputeSize(parent);

        return mSize;
    }

    public Dimension minimumLayoutSize(Container parent) {

        // System.out.println("compute minimum size");
        recomputeSize(parent);

        return mSize;
    }

    public void layoutContainer(Container parent) {

        // do nothing
    }

    private void recomputeSize(Container parent) {
        mSize = new Dimension(0, 0);

        // mSize = parent.getSize();
        // System.out.println("parent size is " + parent.getSize());
        for (Component c : parent.getComponents()) {
            if (c instanceof Node) {

                // System.out.println("Node");
            } else if (c instanceof Edge) {

                // System.out.println("Edge");
            } else if (c instanceof StartSign) {

                // System.out.println("StartSign");
            } else if (c instanceof Comment) {

                // System.out.println("Comment");
            } else if (c instanceof VarBadgeLocal) {

                // System.out.println("VarBadge");
            } else {

                // System.out.println("Unknown");
            }

            // System.out.println("\tat location " + c.getLocation());
            // System.out.println("\thas size" + c.getSize());
            if (c.getLocation().x > mSize.getWidth() - c.getWidth()) {
                mSize.setSize(c.getLocation().x + c.getWidth(), mSize.getHeight());
            }

            if (c.getLocation().y > mSize.getHeight() - c.getHeight()) {
                mSize.setSize(mSize.getWidth(), c.getLocation().y + c.getHeight());
            }
        }

        // System.out.println("SFLM computed size is " + mSize);
    }
}
