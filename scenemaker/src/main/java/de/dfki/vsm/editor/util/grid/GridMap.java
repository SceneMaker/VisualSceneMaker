
/*
* To change this license header, choose License Headers in Project Properties.
* To change this template file, choose Tools | Templates
* and open the template in the editor.
 */
package de.dfki.vsm.editor.util.grid;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.util.grid.pathfinding.PathFindingContext;
import de.dfki.vsm.editor.util.grid.pathfinding.TileBasedMap;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Graphics2D;

/**
 *
 * @author Souza Putra
 */
public class GridMap implements TileBasedMap {
    private int               gridWidth;
    private int               gridHeight;
    private GridRectangle[][] gridWeights;

    public GridMap(GridRectangle[][] gridMap) {
        gridWidth   = gridMap[0].length;
        gridHeight  = gridMap.length;
        gridWeights = gridMap;
    }

    public int getWidthInTiles() {
        return gridWidth;
    }

    public int getHeightInTiles() {
        return gridHeight;
    }

    public void pathFinderVisited(int x, int y) {

        // System.out.println("Visited grid(" + x + " , " + y + ")");
    }

    public boolean blocked(PathFindingContext context, int tx, int ty) {
        return gridWeights[ty][tx].getWeight() < 0;
    }

    public float getCost(PathFindingContext context, int tx, int ty) {
        return gridWeights[ty][tx].getWeight();
    }
}
