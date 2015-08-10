package de.dfki.vsm.editor;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.project.EditorConfig;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.log.LOGDefaultLogger;

import static de.dfki.vsm.Preferences.sSTART_SIGN_COLOR;

//~--- JDK imports ------------------------------------------------------------

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.RenderingHints;

import java.util.Observable;
import java.util.Observer;

import javax.swing.JComponent;

/**
 * @author Patrick Gebhard
 * @author Not me
 */
public class StartSign extends JComponent implements Observer {
    private final LOGDefaultLogger   mLogger      = LOGDefaultLogger.getInstance();
    private final EventDispatcher        mEventCaster = EventDispatcher.getInstance();
    private Point                    mRelPos      = new Point(0, 0);
    private final Color              mColor;
    private final Node               mNode;
    private final boolean            mOutline;
    private final EditorConfig       mEditorConfig;
    private Polygon                  mHead;
    private int                      mHalfHeight;
    private int                      mWidth;
    private int                      mStrokeSize;

    public StartSign(Node node, Point point) {
        mNode    = node;
        mEditorConfig = mNode.getWorkSpace().getEditorConfig();
        
        SetEditorConfig();
        
        mColor   = sSTART_SIGN_COLOR;
      
        mRelPos  = point;
        mOutline = false;
        update();
    }

    public StartSign(Node node, Point point, boolean mode) {
        mEditorConfig = EditorInstance.getInstance().getSelectedProjectEditor().getEditorProject().getEditorConfig();
        SetEditorConfig();
        mColor   = sSTART_SIGN_COLOR;
        mOutline = mode;
        mNode    = node;
        mRelPos  = point;
        update();
    }

    public StartSign(Node node, Point point, boolean mode, Color color) {
        mEditorConfig = EditorInstance.getInstance().getSelectedProjectEditor().getEditorProject().getEditorConfig();
        SetEditorConfig();
        mColor   = color;
        mOutline = mode;
        mNode    = node;
        mRelPos  = point;
        update();
    }

    public void update(Observable o, Object obj) {

        // mLogger.message("StartSign.update(" + obj + ")");
//      update();
    }

    public void update() {
        mHalfHeight = mEditorConfig.sNODEWIDTH / 6;
        mWidth      = mEditorConfig.sNODEWIDTH / 8;
        mStrokeSize = ((mEditorConfig.sNODEWIDTH / 50) < 2
                       ? 2
                       : (mEditorConfig.sNODEWIDTH / 50));
        mHead       = new Polygon();
        mHead.addPoint(2 * mStrokeSize, 2 * mStrokeSize);
        mHead.addPoint(mWidth + 2 * mStrokeSize, mHalfHeight + 2 * mStrokeSize);
        mHead.addPoint(2 * mStrokeSize, mHalfHeight * 2 + 2 * mStrokeSize);
        mHead.addPoint(mWidth / 2 + mStrokeSize, mHalfHeight + 2 * mStrokeSize);
        setSize(mWidth + 4 * mStrokeSize, mHalfHeight * 2 + 4 * mStrokeSize);

        //
        mRelPos = mNode.mDockingManager.occupyDockPointForStartSign();
    }

    private void SetEditorConfig() {
        mHalfHeight = mEditorConfig.sNODEWIDTH / 6;
        mWidth      = mEditorConfig.sNODEWIDTH / 8;
        mStrokeSize = ((mEditorConfig.sNODEWIDTH / 50) < 2
                       ? 2
                       : (mEditorConfig.sNODEWIDTH / 50));
    }

    @Override
    public void paintComponent(java.awt.Graphics g) {
        super.paintComponent(g);

        Graphics2D graphics = (Graphics2D) g;

        graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        //
        setLocation(mNode.getLocation().x - mWidth - 2 * mStrokeSize,
                    mNode.getLocation().y + mEditorConfig.sNODEWIDTH / 2 - mHalfHeight - 2 * mStrokeSize);

        /*
         * mNode.getLocation().x - mRelPos.x - mWidth - 2 * mStrokeSize,
         * mNode.getLocation().y + mRelPos.y - mHalfHeight - 2 * mStrokeSize);
         */

        //
        graphics.setColor(mColor);
        graphics.setStroke(new BasicStroke(mStrokeSize));

        if (mOutline) {
            graphics.drawPolygon(mHead);
        } else {
            graphics.drawPolygon(mHead);
            graphics.fillPolygon(mHead);
        }
    }
}
