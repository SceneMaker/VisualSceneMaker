package communication_package.communication_project;

import org.apache.commons.logging.Log;
import org.ros.message.MessageListener;
import org.ros.namespace.GraphName;
import org.ros.node.*;
import org.ros.node.topic.Subscriber;
import mindbot_msgs.*;

/**
 * A {@link Subscriber} {@link NodeMain} for Mindbot.
 *
 * @author sarah.hoffmann@dfki.de (Sarah Hoffmann)
 */

public class MindbotSubscriber implements NodeMain {
  private Log log;
  private double posexnow = 0;
  private mindbot_msgs.CtrlState statenow;
  private mindbot_msgs.CtrlMode modenow;
  protected Subscriber<geometry_msgs.PoseStamped> subscriberTcpState;
  protected Subscriber<mindbot_msgs.CtrlState> subscriberCtrlState;
  protected Subscriber<mindbot_msgs.CtrlMode> subscriberCtrlMode;
  public static boolean finished;

  @Override
  public GraphName getDefaultNodeName() {
    return GraphName.of("rosjava/mindbotlistener");
  }

  @Override
  public void onStart(ConnectedNode connectedNode) {
    this.log = connectedNode.getLog();
    setupSubscribers(connectedNode);
    setupListeners();
  }

  @Override
  public void onShutdown(Node node) {
    log.info("shutdown node "+ node.getName());
    subscriberCtrlMode.removeAllMessageListeners();
    subscriberCtrlState.removeAllMessageListeners();
    subscriberTcpState.removeAllMessageListeners();
    subscriberCtrlMode.shutdown();
    subscriberCtrlState.shutdown();
    subscriberTcpState.shutdown();
    node.removeListeners();
    node.shutdown();
  }

  @Override
  public void onShutdownComplete(Node node) {

  }

  @Override
  public void onError(Node node, Throwable throwable) {
    log.info("Error: "+ throwable);

  }

  private void setupSubscribers(ConnectedNode connectedNode) {
    subscriberTcpState = connectedNode.newSubscriber("/iiwa/tcp_state", geometry_msgs.PoseStamped._TYPE);
    subscriberCtrlState = connectedNode.newSubscriber("/iiwa/ctrl_state", mindbot_msgs.CtrlState._TYPE);
    subscriberCtrlMode = connectedNode.newSubscriber("/iiwa/ctrl_mode", mindbot_msgs.CtrlMode._TYPE);
  }


  private void setupListeners(){

    subscriberTcpState.addMessageListener(new MessageListener<geometry_msgs.PoseStamped>() {
      @Override
      public void onNewMessage(geometry_msgs.PoseStamped message) {
        double posex = message.getPose().getPosition().getX();
        if (posexnow != posex) {
          posexnow = posex;
          log.info("TcpState Position X: \"" + message.getPose().getPosition().getX() + "\"");
        }
      }
    });

    subscriberCtrlState.addMessageListener(new MessageListener<mindbot_msgs.CtrlState>() {
      public void onNewMessage(mindbot_msgs.CtrlState message) {
        log.info("CtrlState: \"" + message + "\"");
      }
    });

    subscriberCtrlMode.addMessageListener(new MessageListener<mindbot_msgs.CtrlMode>() {
      public void onNewMessage(mindbot_msgs.CtrlMode message) {
        log.info("CtrlMode: \"" + message + "\"");
      }
    });
  }
}
