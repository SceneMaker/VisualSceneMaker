
/*
* To change this license header, choose License Headers in Project Properties.
* To change this template file, choose Tools | Templates
* and open the template in the editor.
 */
package de.dfki.vsm.editor.util.grid.pathfinding;

/**
 * The context describing the current path finding state
 *
 * @author kevin
 */
public interface PathFindingContext {

    /**
     * Get the object being moved along the path if any
     *
     * @return The object being moved along the path
     */
    public Mover getMover();

    /**
     * Get the x coordinate of the source location
     *
     * @return The x coordinate of the source location
     */
    public int getSourceX();

    /**
     * Get the y coordinate of the source location
     *
     * @return The y coordinate of the source location
     */
    public int getSourceY();

    /**
     * Get the distance that has been searched to reach this point
     *
     * @return The distance that has been search to reach this point
     */
    public int getSearchDistance();
}
