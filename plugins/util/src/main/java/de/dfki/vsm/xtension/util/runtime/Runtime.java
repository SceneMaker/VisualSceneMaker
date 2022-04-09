package de.dfki.vsm.xtension.util.runtime;

import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;

public interface Runtime {
    boolean hasVar(String varName);

    AbstractValue getVar(String varName);

    AgentConfig getAgentConfig(String agentName);
}

