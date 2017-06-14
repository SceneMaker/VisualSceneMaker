package de.dfki.vsm.xtension.remote.client.factories.remoteagent;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.xtension.remote.client.agents.RemoteAgent;
import de.dfki.vsm.xtension.remote.client.agents.UIAvatarAgent;
import de.dfki.vsm.xtension.remote.client.sender.DataSendable;

/**
 * Created by alvaro on 6/13/17.
 */
public class UIAvatarAgentFactory extends RemoteAgentAbstractFactory {

    private DataSendable uiAvatarSendable;

    public UIAvatarAgentFactory(AbstractActivity activity) {
        super(activity);
    }

    @Override
    public RemoteAgent createRemoteAgent() {
        String message = clip(activity.get("message"));
        String separator = clip(activity.get("separator"));
        String senderType = clip(activity.get("type"));
        uiAvatarSendable = senderTypeFactory.buildSendable(senderType, message, separator);
        return new UIAvatarAgent(getDataSendable());
    }

    @Override
    public DataSendable getDataSendable() {
        return uiAvatarSendable;
    }
}
