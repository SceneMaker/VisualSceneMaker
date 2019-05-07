package de.dfki.vsm.xtension.remote.client.factories.remoteagent;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.xtension.remote.client.agents.DefaultRemoteAgent;
import de.dfki.vsm.xtension.remote.client.agents.RemoteAgent;
import de.dfki.vsm.xtension.remote.client.sender.DataSendable;
import de.dfki.vsm.xtension.remote.client.senders.StringDefaultSender;
import de.dfki.vsm.xtension.remote.client.senders.decorators.NewLineSendableDecorator;

/**
 * Created by alvaro on 6/13/17.
 */
public class DefaultRemoteFactory extends RemoteAgentAbstractFactory {
    public DefaultRemoteFactory(AbstractActivity activity) {
        super(activity);
    }

    @Override
    public RemoteAgent createRemoteAgent() {
        return new DefaultRemoteAgent(getDataSendable());
    }

    @Override
    public DataSendable getDataSendable() {
        DataSendable sendable =  new StringDefaultSender(activity.getText());
        DataSendable decoratedSendable = new NewLineSendableDecorator(sendable);
        return decoratedSendable;
    }
}
