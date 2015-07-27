
/*
* To change this license header, choose License Headers in Project Properties.
* To change this template file, choose Tools | Templates
* and open the template in the editor.
 */
package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.Edge;
import de.dfki.vsm.editor.Node;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.editor.util.grid.AStarEdgeFinder;
import de.dfki.vsm.editor.util.grid.BezierFit;
import de.dfki.vsm.editor.util.grid.BezierPoint;
import de.dfki.vsm.editor.util.grid.DockingPoint;
import de.dfki.vsm.editor.util.grid.GridConstants;
import de.dfki.vsm.editor.util.grid.GridRectangle;
import de.dfki.vsm.editor.util.grid.pathfinding.Path;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Point2D;

import java.util.ArrayList;
import java.util.Set;

/**
 *
 * @author Souza Putra
 */
public class NormalizeEdgeAction {
    private WorkSpacePanel                 mWorkSpace              = null;
    private de.dfki.vsm.editor.Edge   mGUIEdge                = null;
    protected GridRectangle           gridSource              = null;
    protected GridRectangle           gridDestination         = null;
    protected de.dfki.vsm.editor.Node mSourceGUINode          = null;
    protected de.dfki.vsm.editor.Node mTargetGUINode          = null;
    protected Point                   mSourceGUINodeDockPoint = null;
    protected Point                   mTargetGUINodeDockPoint = null;

    public NormalizeEdgeAction(WorkSpacePanel workSpace, de.dfki.vsm.editor.Edge edge) {
        mWorkSpace     = workSpace;
        mGUIEdge       = edge;
        mSourceGUINode = mGUIEdge.getSourceNode();
        mTargetGUINode = mGUIEdge.getTargetNode();
    }

    public ActionListener getActionListener() {
        return new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                recalculateWeight();
                setEdgePath();

                // mGUIEdge.straightenEdge();
                // renew graphical representation on work space
                mWorkSpace.revalidate();
                mWorkSpace.repaint();
            }
        };
    }

    public void recalculateWeight() {
        mWorkSpace.getGridManager().resetAllGridWeight();

        Set<de.dfki.vsm.editor.Edge> edgeSet = mWorkSpace.getEdges();

        for (de.dfki.vsm.editor.Edge edge : edgeSet) {
            if (!edge.getName().equals(mGUIEdge.getName())) {
                mWorkSpace.getGridManager().setEdgeWeight(edge);
                mWorkSpace.getGridManager().setNodeWeight(edge.getSourceNode());
                mWorkSpace.getGridManager().setNodeWeight(edge.getTargetNode());
            }
        }
    }

//  public void setEdgePath_deprecated() {
//      //if weight of grid intersection is larger than max weight threshold, rerouting needed.
//      if(isReroutingNeeded()) {
//          AStarEdgeFinder aStarPath = new AStarEdgeFinder(mWorkSpace.mGridManager.getmTransitionArea());
//          Path alternatePath = aStarPath.getPath(gridSource.getColumnIndex(), gridSource.getRowIndex(), 
//                  gridDestination.getColumnIndex(), gridDestination.getRowIndex());
//          
////          aStarPath.printPath(gridSource.getColumnIndex(), gridSource.getRowIndex(), 
////                  gridDestination.getColumnIndex(), gridDestination.getRowIndex());
//          
//          // Calculate the control point of the bezier curve that should be made
//          ArrayList<BezierPoint> pathPoints = new ArrayList<BezierPoint>();
//          int deviationX = 0;
//          int deviationY = 0;
//          for(int i = 0; i < alternatePath.getLength(); i++) {
//              BezierPoint point = new BezierPoint(
//                  mWorkSpace.mGridManager.getmTransitionArea()[alternatePath.getY(i)][alternatePath.getX(i)].getCenterX(),
//                  mWorkSpace.mGridManager.getmTransitionArea()[alternatePath.getY(i)][alternatePath.getX(i)].getCenterY());
//              pathPoints.add(point);
//              if(i < alternatePath.getLength()-1) {
//                  deviationX += (alternatePath.getX(i+1) - alternatePath.getX(i));
//                  deviationY += (alternatePath.getY(i+1) - alternatePath.getY(i));
//              }
//          }
//          
//          BezierFit bezierFit = new BezierFit();
//          BezierPoint[] controlPoint = bezierFit.bestFit(pathPoints);
//          
//          //Check direction from target node, this means a vertical movement
//          if(Math.abs(deviationY) > Math.abs(deviationX)) {
//              int deviationVert = 0;
//              for(int i = 0; i < alternatePath.getLength()/3; i++) {
//                  deviationVert += (alternatePath.getX(i+1) - alternatePath.getX(i));
//              }
//              
//              //This means the direction is downward
//              if(deviationVert > 0) {
//                  mSourceGUINodeDockPoint = mSourceGUINode.connectEdgeAtSourceNode(mGUIEdge, new Point(
//                                              mSourceGUINode.getX() + 50, mSourceGUINode.getY() + 100));
//                  mTargetGUINodeDockPoint = mTargetGUINode.connectEdgeAtSourceNode(mGUIEdge, new Point(
//                                              mTargetGUINode.getX() + 50, mTargetGUINode.getY() + 100));
//              }   
//              //This means the direction is upward
//              else {
//                  mSourceGUINodeDockPoint = mSourceGUINode.connectEdgeAtSourceNode(mGUIEdge, new Point(
//                                              mSourceGUINode.getX() + 50, mSourceGUINode.getY())); 
//                  mTargetGUINodeDockPoint = mTargetGUINode.connectEdgeAtSourceNode(mGUIEdge, new Point(
//                                              mTargetGUINode.getX() + 50, mTargetGUINode.getY()));  
//              }
//          }
//          //Check direction from target node, this means a horizontal movement
//          else {
//              int deviationHorz = 0;
//              for(int i = 0; i < alternatePath.getLength()/3; i++) {
//                  deviationHorz += (alternatePath.getY(i+1) - alternatePath.getY(i));
//              }
//              //This means the direction is to the right
//              if(deviationHorz > 0) {
//                  mSourceGUINodeDockPoint = mSourceGUINode.connectEdgeAtSourceNode(mGUIEdge, new Point(
//                                              mSourceGUINode.getX() + 100, mSourceGUINode.getY() + 50));
//                  mTargetGUINodeDockPoint = mTargetGUINode.connectEdgeAtSourceNode(mGUIEdge, new Point(
//                                              mTargetGUINode.getX() + 100, mTargetGUINode.getY() + 50));
//              }
//              //This means the direction is to the left
//              else {
//                  mSourceGUINodeDockPoint = mSourceGUINode.connectEdgeAtSourceNode(mGUIEdge, new Point(
//                                              mSourceGUINode.getX(), mSourceGUINode.getY() + 50));
//                  mTargetGUINodeDockPoint = mTargetGUINode.connectEdgeAtSourceNode(mGUIEdge, new Point(
//                                              mTargetGUINode.getX(), mTargetGUINode.getY() + 50));
//              }
//          }
//          
//          //Manipulate the control point based on the BezierFit calculation
//          mGUIEdge.mEg.mCCrtl1.x = (int) Math.round(controlPoint[1].getX());
//          mGUIEdge.mEg.mCCrtl1.y = (int) Math.round(controlPoint[1].getY());
//          
//          mGUIEdge.mEg.mCCrtl2.x = (int) Math.round(controlPoint[2].getX());
//          mGUIEdge.mEg.mCCrtl2.y = (int) Math.round(controlPoint[2].getY());
//          
//          getEdgeTotalWeight();
//          setGridWeight();
//          mWorkSpace.add(mGUIEdge);
//          mWorkSpace.revalidate();
//          mWorkSpace.repaint();
//      }
//      
//      else {
//          setGridWeight();
//          mWorkSpace.add(mGUIEdge);
//          mWorkSpace.revalidate();
//          mWorkSpace.repaint();
//      }
//  }
    public void setEdgePath() {

        // if weight of grid intersection is larger than max weight threshold, rerouting needed.
        if (isReroutingNeeded()) {

            // System.out.println("Smart Path initiated!");
            AStarEdgeFinder aStarPath = new AStarEdgeFinder(mWorkSpace.mGridManager.getmTransitionArea());

            aStarPath.setDiagonalPathCost(GridConstants.DIAGONAL_PATH_COST);

            Path alternatePath = aStarPath.getPath(gridSource.getColumnIndex(), gridSource.getRowIndex(),
                                     gridDestination.getColumnIndex(), gridDestination.getRowIndex());

//          aStarPath.printPath(gridSource.getColumnIndex(), gridSource.getRowIndex(), 
//                  gridDestination.getColumnIndex(), gridDestination.getRowIndex());
            // Calculate the control point of the bezier curve that should be made
            ArrayList<BezierPoint> pathPoints       = new ArrayList<BezierPoint>();
            int                    deviationSourceX = 0;
            int                    deviationSourceY = 0;
            int                    deviationTargetX = 0;
            int                    deviationTargetY = 0;

            for (int i = 0; i < alternatePath.getLength(); i++) {
                BezierPoint point =
                    new BezierPoint(mWorkSpace.mGridManager
                        .getmTransitionArea()[alternatePath.getY(i)][alternatePath.getX(i)].getCenterX(), mWorkSpace
                        .mGridManager.getmTransitionArea()[alternatePath.getY(i)][alternatePath.getX(i)].getCenterY());

                mWorkSpace.mGridManager.getmTransitionArea()[alternatePath.getY(i)][alternatePath.getX(i)].setaStarPath(
                    true);
                pathPoints.add(point);

                if (i < alternatePath.getLength() / 2 + 2) {
                    deviationSourceX += (alternatePath.getX(i + 1) - alternatePath.getX(i));
                    deviationSourceY += (alternatePath.getY(i + 1) - alternatePath.getY(i));
                } else if ((i >= alternatePath.getLength() / 2 - 2) && (i < alternatePath.getLength() - 1)) {
                    deviationTargetX += (alternatePath.getX(i + 1) - alternatePath.getX(i));
                    deviationTargetY += (alternatePath.getY(i + 1) - alternatePath.getY(i));
                }
            }

            int thresholdSourceX = 0;
            int thresholdSourceY = 0;
            int thresholdTargetX = 0;
            int thresholdTargetY = 0;

            // Indicate vertical movement tendency for source node
            if (Math.abs(deviationSourceX) >= Math.abs(deviationSourceY)) {

                // System.out.println("Vertical movement source." + deviationSourceX + "," + deviationSourceY);
                if (deviationSourceY > 0) {
                    thresholdSourceX = 100;
                } else if (deviationSourceY < 0) {
                    thresholdSourceX = -100;
                }
            }

            // Indicate horizontal movement tendency for source node
            else {

                // System.out.println("Horizontal movement source." + deviationSourceX + "," + deviationSourceY);
                if (deviationSourceX > 0) {
                    thresholdSourceY = 100;
                } else if (deviationSourceX < 0) {
                    thresholdSourceY = -100;
                }
            }

            // Indicate vertical movement tendency for target node
            if (Math.abs(deviationTargetX) >= Math.abs(deviationTargetY)) {

                // System.out.println("Vertical movement target." + deviationTargetX + "," + deviationTargetY);
                if (deviationTargetY > 0) {
                    thresholdTargetX = -100;
                } else if (deviationTargetY < 0) {
                    thresholdTargetX = 100;
                }
            }

            // Indicate horizontal movement tendency for target node
            else {

                // System.out.println("Horizontal movement target." + deviationTargetX + "," + deviationTargetY);
                if (deviationTargetX > 0) {
                    thresholdTargetY = -100;
                } else if (deviationTargetX < 0) {
                    thresholdTargetY = 100;
                }
            }

            BezierFit     bezierFit    = new BezierFit();
            BezierPoint[] controlPoint = bezierFit.bestFit(pathPoints);

            // Manipulate the control point based on the BezierFit calculation
            if (((int) Math.round(controlPoint[1].getX()) + thresholdSourceX) > 0) {
                mGUIEdge.mEg.mCCrtl1.x = (int) Math.round(controlPoint[1].getX()) + thresholdSourceX;
            } else {
                mGUIEdge.mEg.mCCrtl1.x = 50;
            }

            if (((int) Math.round(controlPoint[1].getY()) + thresholdSourceY) > 0) {
                mGUIEdge.mEg.mCCrtl1.y = (int) Math.round(controlPoint[1].getY()) + thresholdSourceY;
            } else {
                mGUIEdge.mEg.mCCrtl1.y = 50;
            }

            if (((int) Math.round(controlPoint[2].getX()) + thresholdTargetX) > 0) {
                mGUIEdge.mEg.mCCrtl2.x = (int) Math.round(controlPoint[2].getX()) + thresholdTargetX;
            } else {
                mGUIEdge.mEg.mCCrtl2.x = 50;
            }

            if (((int) Math.round(controlPoint[2].getY()) + thresholdTargetY) > 0) {
                mGUIEdge.mEg.mCCrtl2.y = (int) Math.round(controlPoint[2].getY()) + thresholdTargetY;
            } else {
                mGUIEdge.mEg.mCCrtl2.y = 50;
            }

            // System.out.println("Control Point 1: " + mGUIEdge.mEg.mCCrtl1.x + "," + mGUIEdge.mEg.mCCrtl1.y);
            // System.out.println("Control Point 2: " + mGUIEdge.mEg.mCCrtl2.x + "," + mGUIEdge.mEg.mCCrtl2.y);
            // getEdgeTotalWeight();
            // setGridWeight();
            DockingPoint sourceDockingPoint = new DockingPoint(mSourceGUINode,
                                                  new Point2D.Double(mGUIEdge.mEg.mCCrtl1.x, mGUIEdge.mEg.mCCrtl1.y));
            DockingPoint targetDockingPoint = new DockingPoint(mTargetGUINode,
                                                  new Point2D.Double(mGUIEdge.mEg.mCCrtl2.x, mGUIEdge.mEg.mCCrtl2.y));

            if ((sourceDockingPoint.getIntersectionX() > -1) && (sourceDockingPoint.getIntersectionY() > -1)) {
                mSourceGUINode.disconnectEdge(mGUIEdge);
                mSourceGUINodeDockPoint = mSourceGUINode.connectEdgeAtSourceNode(mGUIEdge,
                        new Point(sourceDockingPoint.getIntersectionX(), sourceDockingPoint.getIntersectionY()));
                this.mWorkSpace.mGridManager.addDockingPoints(mSourceGUINodeDockPoint);
            }

            if ((targetDockingPoint.getIntersectionX() > -1) && (targetDockingPoint.getIntersectionY() > -1)) {
                mTargetGUINode.disconnectEdge(mGUIEdge);
                mTargetGUINodeDockPoint = mTargetGUINode.connectEdgetAtTargetNode(mGUIEdge,
                        new Point(targetDockingPoint.getIntersectionX(), targetDockingPoint.getIntersectionY()));
                this.mWorkSpace.mGridManager.addDockingPoints(mTargetGUINodeDockPoint);
            }

            mWorkSpace.add(mGUIEdge);
            mWorkSpace.revalidate();
            mWorkSpace.repaint();
        } else {

            // setGridWeight();
            mWorkSpace.add(mGUIEdge);
            mWorkSpace.revalidate();
            mWorkSpace.repaint();
        }
    }

    public boolean isReroutingNeeded() {
        return getEdgeTotalWeight() >= GridConstants.MAX_WEIGHT_THRESHOLD;
    }

    public int getEdgeTotalWeight() {
        int sumWeight = 0;

        // Determining the positioning of edge's anchor. False means source has
        // smaller coordinate than destination
        boolean anchorMode = false;

        if ((mSourceGUINode.getX() >= mTargetGUINode.getX()) || (mSourceGUINode.getY() >= mTargetGUINode.getY())) {
            anchorMode = true;
        }

        gridSource      = null;
        gridDestination = null;

        for (GridRectangle[] gridParent : mWorkSpace.mGridManager.getmTransitionArea()) {
            for (GridRectangle gridRectangle : gridParent) {
                gridRectangle.setaStarPath(false);

                if (gridRectangle.isIntersectedbyNode(mSourceGUINode)) {
                    gridRectangle.setIntersectionType(GridRectangle.NODE_INTERSECTION);
                    sumWeight += gridRectangle.getWeight();

                    if (anchorMode) {
                        if (gridSource == null) {
                            gridSource = gridRectangle;
                        }
                    } else {
                        gridSource = gridRectangle;
                    }
                }

                if (gridRectangle.isIntersectedbyNode(mTargetGUINode)) {
                    gridRectangle.setIntersectionType(GridRectangle.NODE_INTERSECTION);
                    sumWeight += gridRectangle.getWeight();

                    if (anchorMode) {
                        gridDestination = gridRectangle;
                    } else {
                        if (gridDestination == null) {
                            gridDestination = gridRectangle;
                        }
                    }
                }

                if (gridRectangle.isIntersectByRectangle(mGUIEdge.mEg)) {
                    gridRectangle.setIntersectionType(GridRectangle.EDGE_INTERSECTION);
                    sumWeight += gridRectangle.getWeight();
                }
            }
        }

        return sumWeight;
    }

//  public void setGridWeight() {
//      mWorkSpace.getGridManager().setEdgeWeight(mGUIEdge);
//      mWorkSpace.getGridManager().setNodeWeight(mSourceGUINode);
//      mWorkSpace.getGridManager().setNodeWeight(mTargetGUINode);
//  }
}
