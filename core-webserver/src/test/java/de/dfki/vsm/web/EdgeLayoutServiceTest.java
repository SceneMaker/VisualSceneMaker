package de.dfki.vsm.web;

import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.model.sceneflow.chart.edge.TimeoutEdge;
import de.dfki.vsm.model.sceneflow.chart.graphics.node.NodeGraphics;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

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

    /**
     * An edge that arrived without geometry gets some, and a self-loop gets an arc that leaves the
     * node.
     *
     * <p>A generated flow carries no drawing at all. Without this, a timeout edge from a node back to
     * itself is a line of zero length under the node: it runs, and it cannot be seen, so the only way
     * to find it is to relayout the whole flow by hand.
     */
    @Test
    void anEdgeArrivingWithoutGeometryIsGivenSomeAndASelfLoopArcsAway() {
        SceneFlow flow = new SceneFlow();
        flow.setId("SceneFlow");
        BasicNode node = new BasicNode();
        node.setId("N1");
        node.setName("Waiting");
        node.setGraphics(new NodeGraphics(200, 300));
        node.setParentNode(flow);
        flow.addNode(node);

        TimeoutEdge selfLoop = new TimeoutEdge();
        selfLoop.setTargetUnid("N1");
        selfLoop.setSourceUnid("N1");
        selfLoop.setTargetNode(node);
        selfLoop.setSourceNode(node);
        selfLoop.setTimeout(1000);
        node.setDedge(selfLoop);

        EdgeLayoutService service = new EdgeLayoutService();
        assertEquals(1, service.layoutEdgesWithoutGeometry(flow, 90, 90));

        assertNotNull(selfLoop.getGraphics());
        var points = selfLoop.getGraphics().getConnection().getPointList();
        assertEquals(2, points.size(), "An edge needs both ends before it can be drawn");
        assertTrue(points.get(0).getCtrlYPos() < points.get(0).getYPos(),
                "The arc has to leave the node, or the edge stays hidden behind it");

        assertEquals(0, service.layoutEdgesWithoutGeometry(flow, 90, 90),
                "An edge that already has geometry must be left exactly as the author arranged it");
    }
}
