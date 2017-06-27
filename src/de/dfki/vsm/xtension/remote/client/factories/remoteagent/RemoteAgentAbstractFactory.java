package de.dfki.vsm.xtension.remote.client.factories.remoteagent;

import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.xtension.remote.client.agents.RemoteAgent;
import de.dfki.vsm.xtension.remote.client.factories.SenderTypeFactory;
import de.dfki.vsm.xtension.remote.client.sender.DataSendable;

/**
 * Created by alvaro on 6/13/17.
 */
public abstract class RemoteAgentAbstractFactory {
    protected final AbstractActivity activity;
    protected SenderTypeFactory senderTypeFactory =  new SenderTypeFactory();

    RemoteAgentAbstractFactory(AbstractActivity activity){
        senderTypeFactory =  new SenderTypeFactory();
        this.activity = activity;
    }

    public static RemoteAgent createRemoteAgent(AgentConfig agentConfig, AbstractActivity activity){
        RemoteAgentAbstractFactory factory;
        if(agentConfig.getProperty("remote_type").equals("uiavatar")){
            factory = new UIAvatarAgentFactory(activity);
        }else{
            factory = new DefaultRemoteFactory(activity);
        }
        return factory.createRemoteAgent();
    }

    public abstract RemoteAgent createRemoteAgent();

    public abstract DataSendable getDataSendable();

    String clip(String message) {
        if(message != null){
            return message.replace("'", "");
        }
        return "";
    }
}
