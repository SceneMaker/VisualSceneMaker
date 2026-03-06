package de.dfki.vsm.web;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class EdgeLayoutServiceTest {

    @Test
    void prefersCenterAlignedDockPointsWhenShortestDistanceIsTied() {
        EdgeLayoutService service = new EdgeLayoutService();

        int[] pair = service.findBestDockPointPair(
                "S4", 120, 480, 90, 90, true,
                "S2", 390, 480, 90, 90, true
        );

        assertEquals(18, pair[0], "source dock should be the center-right point");
        assertEquals(6, pair[1], "target dock should be the center-left point");
    }

    @Test
    void fallsBackToNearestAvailableAlignedDockPointsWhenCenterIsOccupied() {
        EdgeLayoutService service = new EdgeLayoutService();
        service.occupyDockPoint("S4", 18, true);
        service.occupyDockPoint("S2", 6, false);

        int[] pair = service.findBestDockPointPair(
                "S4", 120, 480, 90, 90, true,
                "S2", 390, 480, 90, 90, true
        );

        assertEquals(17, pair[0], "source dock should move to the next closest right-side point");
        assertEquals(7, pair[1], "target dock should move to the next closest left-side point");
    }
}
