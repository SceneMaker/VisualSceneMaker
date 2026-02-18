package de.dfki.vsm.runtime.api.android;

import de.dfki.vsm.runtime.api.RuntimeCommandEndpoint;
import de.dfki.vsm.runtime.project.RunTimeProject;
import org.json.JSONObject;

/**
 * Runtime endpoint contract for Android HTTP/WS hosting.
 */
public interface AndroidRuntimeApi extends RuntimeCommandEndpoint {

    String projectId();

    String projectName();

    String projectPath();

    String runtimeState();

    JSONObject runtimeSnapshot();

    RunTimeProject runtimeProject();
}
