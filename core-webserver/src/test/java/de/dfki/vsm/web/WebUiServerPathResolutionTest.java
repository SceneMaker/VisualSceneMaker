package de.dfki.vsm.web;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class WebUiServerPathResolutionTest {

    @Test
    void appendsProjectNameToBaseDirectory() {
        assertEquals("/tmp/projects/Demo", WebUiServer.resolveProjectDirectory("/tmp/projects", "Demo"));
    }

    @Test
    void avoidsAppendingProjectNameTwice() {
        assertEquals("/tmp/projects/Demo", WebUiServer.resolveProjectDirectory("/tmp/projects/Demo", "Demo"));
    }

    @Test
    void resolvesProjectXmlToProjectDirectoryBeforeAppending() {
        assertEquals("/tmp/projects/Demo", WebUiServer.resolveProjectDirectory("/tmp/projects/project.xml", "Demo"));
    }
}
