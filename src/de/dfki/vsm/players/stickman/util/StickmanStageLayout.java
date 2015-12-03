package de.dfki.vsm.players.stickman.util;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.LayoutManager;

/**
 *
 * @author Patrick Gebahrd
 *
 */
public class StickmanStageLayout implements LayoutManager {

	private double sScale = 1.0d;

	private int minWidth = 0, minHeight = 0;
	private int preferredWidth = 0, preferredHeight = 0;
	private boolean sizeUnknown = true;

	public StickmanStageLayout() {
		//sScale = scale;
	}

//	public void setScale(double scale) {
//		sScale = scale;
//	}
//
//	private int scale(int input) {
//		return new Double(input * sScale).intValue();
//	}

	@Override
	public void addLayoutComponent(String name, Component comp) {
	}

	@Override
	public void removeLayoutComponent(Component comp) {
	}

	private void setSizes(Container parent) {
		Dimension d;

		preferredWidth = 0;
		preferredHeight = 0;
		minWidth = 0;
		minHeight = 0;

		for (Component c : parent.getComponents()) {
			if (c.isVisible()) {
				d = c.getPreferredSize();
				preferredWidth += d.width;
				preferredHeight = Math.max(d.height, preferredHeight);

				minWidth = preferredWidth;
				minHeight = preferredHeight;
			}
		}
	}

	@Override
	public Dimension preferredLayoutSize(Container parent) {
		Dimension d = new Dimension(0, 0);

		setSizes(parent);

		Insets insets = parent.getInsets();
		d.width = preferredWidth + insets.left + insets.right;;
		d.height = preferredHeight + insets.top + insets.bottom;;

		sizeUnknown = false;

		return d;
	}

	@Override
	public Dimension minimumLayoutSize(Container parent) {
		return preferredLayoutSize(parent);
	}


	/*
	 * provides a flow layout with letting the subcomponents size untouched
	 */
	@Override
	public void layoutContainer(Container parent) {
		Insets insets = parent.getInsets();
		int previousWidth = 0, previousHeight = 0;
		int x = 0, y = insets.top;

		if (sizeUnknown) {
			setSizes(parent);
		}

		for (int i = 0; i < parent.getComponentCount(); i++) {
			Component c = parent.getComponent(i);

			if (c.isVisible()) {
				Dimension d = c.getPreferredSize();

				if (i > 0) {
					x += previousWidth;
				}

				//c.setBounds(x, y, (sScale > 1.0d) ? scale(d.width) : d.width, (sScale > 1.0d) ? scale(d.height) : d.height);
				c.setBounds(x, y, d.width, d.height);

				previousWidth = d.width;
				previousHeight = d.height;
			}
		}
	}
}
