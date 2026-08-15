package de.dfki.vsm.sceneflow.ir;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class EdgeLabelMapperTest {

    @Test
    void mapsKnownSynonymsToHumanLabels() {
        assertEquals("timeout edge", EdgeLabelMapper.toHumanLabel("TEDGE"));
        assertEquals("timeout edge", EdgeLabelMapper.toHumanLabel("tedge"));
        assertEquals("timeout edge", EdgeLabelMapper.toHumanLabel("t-edge"));

        assertEquals("unconditional edge", EdgeLabelMapper.toHumanLabel("EEDGE"));
        assertEquals("conditional edge", EdgeLabelMapper.toHumanLabel("c-edge"));
        assertEquals("probabilistic edge", EdgeLabelMapper.toHumanLabel("PEDGE"));
        assertEquals("fork edge", EdgeLabelMapper.toHumanLabel("fedge"));
        assertEquals("interrupt edge", EdgeLabelMapper.toHumanLabel("IEDGE"));
        assertEquals("interrupt edge", EdgeLabelMapper.toHumanLabel("i-edge"));
        assertEquals("interrupt edge", EdgeLabelMapper.toHumanLabel("IEGDE"));
    }

    @Test
    void fallsBackGracefullyForUnknownOrEmpty() {
        assertEquals("edge", EdgeLabelMapper.toHumanLabel(""));
        assertEquals("edge", EdgeLabelMapper.toHumanLabel(null));
        assertEquals("xedge", EdgeLabelMapper.toHumanLabel("XEDGE"));
    }
}
