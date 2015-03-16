package de.dfki.vsm.editor;

import de.dfki.vsm.util.ios.ResourceLoader;
import java.awt.Cursor;
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
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.TransferHandler;

public final class ToolBarItem extends JButton implements Transferable{

    private final String mText;
    private final Icon mIcon;
    private final Object mData;
    //marks this JButton as the source of the Drag
    private DragSource source;
    private TransferHandler t;
    private final DragGestureListener mDragGestureListener;
    private final DragSourceListener mDragSourceListener;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public ToolBarItem(
            final String text,
            final String icon,
            final Object data) {
        mText = text;
        mIcon = ResourceLoader.loadImageIcon("/res/img/elementtree/" + icon + ".png");
        mData = data;
        setContentAreaFilled(false);
        setFocusable(false);
        setOpaque(false);
        //to be transferred in the Drag
        t = new TransferHandler(text);
        setTransferHandler(t);
        
//        addMouseListener(new MouseAdapter() {
//            public void mousePressed(MouseEvent e) {
////                JComponent c = (JComponent) e.getSource();
////                TransferHandler handler = c.getTransferHandler();
////                handler.exportAsDrag(c, e, TransferHandler.COPY);
//                Image ic = ResourceLoader.loadImageIcon("/res/img/elementtree/" + icon + ".png").getImage();
//                setCursor(Toolkit.getDefaultToolkit().createCustomCursor(ic, new Point(15, 15), text));
//            }
//
//            public void mouseReleased(MouseEvent e) {
//                //e.get
//                setCursor(java.awt.Cursor.getDefaultCursor());
//            }
//        });
        mDragSourceListener = new DragSourceListener() {

            @Override
            public void dragEnter(DragSourceDragEvent dsde) {
                System.out.println("drag enter");
            }

            @Override
            public void dragOver(DragSourceDragEvent dsde) {
                System.out.println("drag over");
            }

            @Override
            public void dropActionChanged(DragSourceDragEvent dsde) {
                System.out.println("drag action changed");
            }

            @Override
            public void dragExit(DragSourceEvent dse) {
                System.out.println("drag exit");
            }

            @Override
            public void dragDropEnd(DragSourceDropEvent dsde) {
                System.out.println("drag drop end");
            }
        };
        mDragGestureListener = new DragGestureListener() {

            @Override
            public void dragGestureRecognized(DragGestureEvent event) {
                Image cursorIcon = ResourceLoader.loadImageIcon("/res/img/elementtree/" + icon + ".png").getImage();
                Cursor cur = Toolkit.getDefaultToolkit().createCustomCursor(cursorIcon, new Point(15, 15), text);
                source.startDrag(event, cur, (ToolBarItem)event.getComponent(), mDragSourceListener);
            }
        };
        //The Drag will copy the DnDButton rather than moving it
        source = new DragSource();
        source.createDefaultDragGestureRecognizer(this, DnDConstants.ACTION_COPY, mDragGestureListener);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final String getText() {
        return mText;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final Icon getIcon() {
        return mIcon;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final Object getData() {
        return mData;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final Object getTransferData(final DataFlavor flavor) throws
            UnsupportedFlavorException, IOException {
        return mData;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final boolean isDataFlavorSupported(final DataFlavor flavor) {
        return true;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final DataFlavor[] getTransferDataFlavors() {
        return null;
    }
}
