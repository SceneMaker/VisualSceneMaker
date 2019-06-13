
/*
* To change this template, choose Tools | Templates
* and open the template in the editor.
 */
package de.dfki.vsm.editor.util;

//~--- non-JDK imports --------------------------------------------------------

import java.awt.*;

//~--- JDK imports ------------------------------------------------------------

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
