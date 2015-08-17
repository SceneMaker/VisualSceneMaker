package de.dfki.vsm.editor.project.sceneflow.elements;

import de.dfki.vsm.util.ios.ResourceLoader;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Image;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragSource;
import java.awt.dnd.DragSourceDragEvent;
import java.awt.dnd.DragSourceDropEvent;
import java.awt.dnd.DragSourceEvent;
import java.awt.dnd.DragSourceListener;

import java.io.IOException;

import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.SwingConstants;
import javax.swing.TransferHandler;

public final class SceneFlowPaletteItem extends JButton implements Transferable {

    // TODO: get icons path from prefrences
    private final String mIconsPath = "/res/img/workspace_toolbar/";
    // TODO: do that to static preferences
    private final Dimension mToolItemSize = new Dimension(61, 65);
    private final String mToolTipText;
    private final Icon mStandardIcon;
    private final Icon mRollOverIcon;
    private final Object mDragableData;

    // Drag & drop support
    private final DragSource mDragSource;
    private final TransferHandler mTransferHandler;
    private final DragGestureListener mDragGestureListener;
    private final DragSourceListener mDragSourceListener;

    // Create a sceneflow element item
    public SceneFlowPaletteItem(
            final String text,
            final String info,
            final String icon,
            final Object data) {
        mToolTipText = text;
        mStandardIcon = ResourceLoader.loadImageIcon(mIconsPath + icon + ".png");
        mRollOverIcon = ResourceLoader.loadImageIcon(mIconsPath + icon + "_BLUE.png");
        mDragableData = data;
        setContentAreaFilled(false);
        setFocusable(false);
        setOpaque(false);
        setBorder(null);

        // to be transferred in the Drag
        mTransferHandler = new TransferHandler(text);
        setTransferHandler(mTransferHandler);
        setVerticalTextPosition(SwingConstants.BOTTOM);
        setHorizontalTextPosition(SwingConstants.CENTER);
        setToolTipText(mToolTipText + ": " + info);
        setPreferredSize(mToolItemSize);
        setMinimumSize(mToolItemSize);
        setMaximumSize(mToolItemSize);
        mDragSourceListener = new DragSourceListener() {
            @Override
            public void dragEnter(DragSourceDragEvent dsde) {

                //System.out.println("drag enter");
            }

            @Override
            public void dragOver(DragSourceDragEvent dsde) {

                //System.out.println("drag over");
            }

            @Override
            public void dropActionChanged(DragSourceDragEvent dsde) {
                //System.out.println("drag action changed");
            }

            @Override
            public void dragExit(DragSourceEvent dse) {

                //System.out.println("drag exit");
            }

            @Override
            public void dragDropEnd(DragSourceDropEvent dsde) {

                //System.out.println("drag drop end");
            }
        };
        mDragGestureListener = new DragGestureListener() {
            @Override
            public void dragGestureRecognized(DragGestureEvent event) {
                Image cursorIcon = ResourceLoader.loadImageIcon(mIconsPath + icon + "_SMALL.png").getImage();
                Cursor cur = Toolkit.getDefaultToolkit().createCustomCursor(cursorIcon, new Point(0, 0), mToolTipText);

                mDragSource.startDrag(event, cur, (SceneFlowPaletteItem) event.getComponent(), mDragSourceListener);
            }
        };

        // The Drag will copy the DnDButton rather than moving it
        mDragSource = new DragSource();
        mDragSource.createDefaultDragGestureRecognizer(this, DnDConstants.ACTION_COPY, mDragGestureListener);
    }

    // Get the data for a drag & drop operation
    @Override
    public final Object getTransferData(final DataFlavor flavor) throws UnsupportedFlavorException, IOException {
        return mDragableData;
    }

    // Generally support all d&d data flavours
    @Override
    public final boolean isDataFlavorSupported(final DataFlavor flavor) {
        return true;
    }

    //
    @Override
    public final DataFlavor[] getTransferDataFlavors() {
        return null;
    }

    @Override
    public final Icon getIcon() {
        return mStandardIcon;
    }

    @Override
    public final Icon getRolloverIcon() {
        return mRollOverIcon;
    }
}
