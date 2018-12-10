
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
 * A heuristic that drives the search based on the Manhattan distance
 * between the current location and the target
 *
 * @author Kevin Glass
 */
public class ManhattanHeuristic implements AStarHeuristic {

    /** The minimum movement cost from any one square to the next */
    private int minimumCost;

    /**
     * Create a new heuristic
     *
     * @param minimumCost The minimum movement cost from any one square to the next
     */
    public ManhattanHeuristic(int minimumCost) {
        this.minimumCost = minimumCost;
    }

    /**
     * @see AStarHeuristic#getCost(TileBasedMap, Mover, int, int, int, int)
     */
    public float getCost(TileBasedMap map, Mover mover, int x, int y, int tx, int ty) {
        return minimumCost * (Math.abs(x - tx) + Math.abs(y - ty));
    }
}
