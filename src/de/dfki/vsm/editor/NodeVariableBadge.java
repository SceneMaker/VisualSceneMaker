package de.dfki.vsm.editor;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.model.project.EditorConfig;
import de.dfki.vsm.util.TextFormat;
import de.dfki.vsm.util.tpl.TPLTuple;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.font.TextLayout;

import java.text.AttributedString;

import java.util.ArrayList;

import javax.swing.JComponent;

/**
 * @author Not me
 * @author Patrick Gebhard
 */
public class NodeVariableBadge extends JComponent {
    LocationType                                  mLocation          = LocationType.RIGHT;
    Node                                          mNode              = null;
    WorkSpacePanel                                     mWorkSpace         = null;
    de.dfki.vsm.model.sceneflow.Node              mDataNode          = null;
    Point                                         mRelPos            = null;
    ArrayList<TPLTuple<String, AttributedString>> mLocalVarDefList   = null;
    ArrayList<TPLTuple<String, AttributedString>> mGlobalVarDefList  = null;
    ArrayList<TPLTuple<String, AttributedString>> mLocalTypeDefList  = null;
    ArrayList<TPLTuple<String, AttributedString>> mGlobalTypeDefList = null;
    ArrayList<TPLTuple<String, AttributedString>> mCompleteList      = new ArrayList<TPLTuple<String,
                                                                           AttributedString>>();
    int                                           mPositionOffset    = 10;
    int                                           mBeautyXOffSet     = 0;
    int                                           mBeautyYOffSet     = 0;
    EditorConfig                            mEditorConfig;

    static enum LocationType { TOP, BOTTOM, LEFT, RIGHT }

    ;
    public NodeVariableBadge(Node node, WorkSpacePanel workSpace, ArrayList<String> localVarDefList,
                             ArrayList<String> globalVarDefList, ArrayList<String> localTypeDefList,
                             ArrayList<String> globalTypeDefList) {
        mNode              = node;
        mEditorConfig       = EditorInstance.getInstance().getSelectedProjectEditor().getEditorProject().getEditorConfig();
        mDataNode          = node.getDataNode();
        mWorkSpace         = workSpace;
        mLocalVarDefList   = TextFormat.getPairList(localVarDefList);
        mGlobalVarDefList  = TextFormat.getPairList(globalVarDefList);
        mLocalTypeDefList  = TextFormat.getPairList(localTypeDefList);
        mGlobalTypeDefList = TextFormat.getPairList(globalTypeDefList);
        mCompleteList.addAll(mGlobalTypeDefList);
        mCompleteList.addAll(mLocalTypeDefList);
        mCompleteList.addAll(mGlobalVarDefList);
        mCompleteList.addAll(mLocalVarDefList);

        // Set Size, Location and Correct Bounds
        setSize(new Dimension(1, 1));
        setLocation(computeLocation());

        // correctBounds();
    }

    private Point computeLocation() {

        // compute location
        mRelPos = new Point(mNode.getLocation());

        int width    = getSize().width;
        int height   = getSize().height;
        int wSWidth  = mWorkSpace.getSize().width;
        int wSHeight = mWorkSpace.getSize().height;

        // check where variable badge can be placed
        int leftX   = mRelPos.x - width - mPositionOffset;
        int topY    = mRelPos.y - height - mPositionOffset;
        int rightX  = mRelPos.x + mEditorConfig.sNODEWIDTH + mPositionOffset;
        int bottomY = mRelPos.x + mEditorConfig.sNODEHEIGHT + mPositionOffset;

        if (rightX + width < wSWidth) {              // right
            mRelPos.x = rightX;
            mRelPos.y = mRelPos.y - ((height - mEditorConfig.sNODEHEIGHT) / 2);
            mLocation = LocationType.RIGHT;
        } else if (leftX > 0) {                      // left
            mRelPos.x = leftX;
            mRelPos.y = mRelPos.y - ((height - mEditorConfig.sNODEHEIGHT) / 2);
            mLocation = LocationType.LEFT;
        } else if (bottomY + height < wSHeight) {    // bottom
            mRelPos.x = mRelPos.x - ((width - mEditorConfig.sNODEWIDTH) / 2);
            mRelPos.y = bottomY;
            mLocation = LocationType.BOTTOM;
        } else if (topY > 0) {                       // top
            mRelPos.x = mRelPos.x - ((width - mEditorConfig.sNODEWIDTH) / 2);
            mRelPos.y = topY;
            mLocation = LocationType.TOP;
        } else {                                     // left - default
            mRelPos.x = leftX;
            mRelPos.y = mRelPos.y - ((height - mEditorConfig.sNODEHEIGHT) / 2);
            mLocation = LocationType.LEFT;
        }

        return mRelPos;
    }

    private Dimension computeTextRectSize(Graphics2D graphics) {
        int width  = 0,
            height = 0;

        for (int i = 0; i < mCompleteList.size(); i++) {
            TextLayout textLayout = new TextLayout(mCompleteList.get(i).getSecond().getIterator(),
                                        graphics.getFontRenderContext());
            int advance = (int) textLayout.getVisibleAdvance();

            if (advance > width) {
                width = advance;
            }

            int currentAll = (int) (textLayout.getAscent() + textLayout.getDescent() + textLayout.getLeading());

            height = height + currentAll;
        }

        return new Dimension(width + 2 * mPositionOffset, height);
    }

    private void correctBounds() {
        Point     newStartPos;
        Point     oldStartPos;
        Dimension oldSize;

        switch (mLocation) {
        case RIGHT :
            newStartPos = mNode.getCenterPoint();
            oldStartPos = getLocation();
            oldSize     = getSize();
            setLocation(newStartPos.x, oldStartPos.y);
            mBeautyXOffSet = oldStartPos.x - newStartPos.x;
            mBeautyYOffSet = 0;
            setSize(oldSize.width + mBeautyXOffSet, oldSize.height);

            break;

        case LEFT :

            // TODO
            break;

        case TOP :

            // TODO
            break;

        case BOTTOM :

            // TODO
            break;
        }
    }

    @Override
    public void paintComponent(java.awt.Graphics g) {
        super.paintComponent(g);

        Graphics2D graphics = (Graphics2D) g;

        graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        // Set Size, Location and Correct Bounds
        Dimension dimension = computeTextRectSize(graphics);

        setSize(dimension);
        setLocation(computeLocation());

        // correctBounds();
        // Draw Background Rectangle
        graphics.setColor(new Color(110, 110, 110, 110));
        graphics.fillRoundRect(0, 0, dimension.width, dimension.height, 5, 5);

        // Draw Type Definitions and Variable Definition
        int currentDrawingOffset = 0;

        for (TPLTuple<String, AttributedString> pair : mCompleteList) {
            AttributedString attributedString = pair.getSecond();
            TextLayout       textLayout       = new TextLayout(attributedString.getIterator(),
                                                    graphics.getFontRenderContext());

            currentDrawingOffset = currentDrawingOffset + (int) textLayout.getAscent();
            graphics.drawString(attributedString.getIterator(), mPositionOffset, currentDrawingOffset);
            currentDrawingOffset = currentDrawingOffset + (int) textLayout.getLeading() + (int) textLayout.getDescent();
        }
    }
}
