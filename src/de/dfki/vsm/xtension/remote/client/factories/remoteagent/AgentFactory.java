package de.dfki.vsm.xtension.remote.client.factories.remoteagent;

import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.xtension.remote.client.agents.RemoteAgent;

/**
 * Created by alvaro on 6/13/17.
 */
public class AgentFactory {
    public static RemoteAgent createRemoteAgent(AgentConfig agentConfig, AbstractActivity activity){
        RemoteAgentAbstractFactory factory;
        if(agentConfig.getProperty("remote_type").equals("uiavatar")){
            factory = new UIAvatarAgentFactory(activity);
        }else{
            factory = new DefaultRemoteFactory(activity);
        }
        return factory.createRemoteAgent();
    }




}
