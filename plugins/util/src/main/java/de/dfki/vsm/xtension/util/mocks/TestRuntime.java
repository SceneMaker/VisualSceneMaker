package de.dfki.vsm.xtension.util.mocks;

import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.xtension.util.runtime.DrivenRuntime;

public class TestRuntime implements DrivenRuntime {
    @Override
    public void setVar(String name, AbstractValue value) {

    }

    @Override
    public void wait(String vmuid) {

    }

    @Override
    public void handleMarker(String marker) {

    }

    @Override
    public void stopWaiting(String vmuid) {

    }

    @Override
    public void purgeActivities() {

    }

    @Override
    public boolean hasVar(String varName) {
        return false;
    }

    @Override
    public AbstractValue getVar(String varName) {
        return null;
    }

    @Override
    public AgentConfig getAgentConfig(String agentName) {
        return null;
    }
}
