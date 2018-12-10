
/*
* To change this license header, choose License Headers in Project Properties.
* To change this template file, choose Tools | Templates
* and open the template in the editor.
 */
package de.dfki.vsm.editor.util.grid;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.util.grid.pathfinding.AStarPathFinder;
import de.dfki.vsm.editor.util.grid.pathfinding.Path;

/**
 * This is a class for finding alternate route due to line intersection.
 * @author Souza Putra
 */
public class AStarEdgeFinder {
    private static final int MAX_PATH_LENGTH = 100;
    private int              diagonalCost    = 0;
    private GridMap          gridMap;

    public AStarEdgeFinder(GridRectangle[][] gridRectangle) {
        this.gridMap = new GridMap(gridRectangle);
    }

    public Path getPath(int sourceX, int sourceY, int destinationX, int destinationY) {
        AStarPathFinder pathFinder = new AStarPathFinder(gridMap, MAX_PATH_LENGTH, true);

        pathFinder.setDiagonalPathCost(this.diagonalCost);

        return pathFinder.findPath(null, sourceX, sourceY, destinationX, destinationY);
    }

    public void printPath(int sourceX, int sourceY, int destinationX, int destinationY) {
        AStarPathFinder pathFinder = new AStarPathFinder(gridMap, MAX_PATH_LENGTH, true);
        Path            path       = pathFinder.findPath(null, sourceX, sourceY, destinationX, destinationY);

        pathFinder.setDiagonalPathCost(this.diagonalCost);

        for (int i = 0; i < path.getLength(); i++) {

            // System.out.println("Move to: " + path.getX(i) + "," + path.getY(i) + ".");
        }
    }

    public void setDiagonalPathCost(int costValue) {
        this.diagonalCost = costValue;
    }

    public int getDiagonalPathCost() {
        return this.diagonalCost;
    }
}
