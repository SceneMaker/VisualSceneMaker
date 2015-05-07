
/*
* To change this license header, choose License Headers in Project Properties.
* To change this template file, choose Tools | Templates
* and open the template in the editor.
 */
package de.dfki.vsm.editor.util.grid.pathfinding.heuristics;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.util.grid.pathfinding.AStarHeuristic;
import de.dfki.vsm.editor.util.grid.pathfinding.Mover;
import de.dfki.vsm.editor.util.grid.pathfinding.TileBasedMap;

/**
 * A heuristic that uses the tile that is closest to the target
 * as the next best tile.
 *
 * @author Kevin Glass
 */
public class ClosestHeuristic implements AStarHeuristic {

    /**
     * @see AStarHeuristic#getCost(TileBasedMap, Mover, int, int, int, int)
     */
    public float getCost(TileBasedMap map, Mover mover, int x, int y, int tx, int ty) {
        float dx     = tx - x;
        float dy     = ty - y;
        float result = (float) (Math.sqrt((dx * dx) + (dy * dy)));

        return result;
    }
}
