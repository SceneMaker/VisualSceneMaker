package communication_package.communication_project;

import mindbot_msgs.*;
import org.apache.commons.logging.Log;
import org.ros.namespace.GraphName;
import org.ros.node.ConnectedNode;
import org.ros.node.Node;
import org.ros.node.NodeMain;
import org.ros.node.topic.DefaultPublisherListener;
import org.ros.node.topic.Publisher;

/**
 * A {@link Publisher} {@link NodeMain} for Mindbot.
 *
 * @author sarah.hoffmann@dfki.de (Sarah Hoffmann)
 */

public class MindbotPublisher implements NodeMain {

  private Log log;

  @Override
  public GraphName getDefaultNodeName() {
    return GraphName.of("rosjava/MindbotPublisher");
  }

  @Override
  public void onStart(final ConnectedNode connectedNode) {
    this.log = connectedNode.getLog();

    // Publish the new TCP Target
    final Publisher<geometry_msgs.Pose> publishertcp = connectedNode.newPublisher("/iiwa/set_tcp_target", geometry_msgs.Pose._TYPE);
    if (publishertcp.hasSubscribers()) {
      geometry_msgs.Pose poseJointTarget = publishertcp.newMessage();
      poseJointTarget.getPosition().setX(3);
      poseJointTarget.getPosition().setY(1);
      poseJointTarget.getPosition().setZ(1);
      poseJointTarget.getOrientation().setW(1);
      poseJointTarget.getOrientation().setX(1);
      poseJointTarget.getOrientation().setY(1);
      poseJointTarget.getOrientation().setZ(1);
      publishertcp.publish(poseJointTarget);
      publishertcp.addListener(new DefaultPublisherListener<geometry_msgs.Pose>() {
        @Override
        public void onMasterRegistrationFailure(Publisher<geometry_msgs.Pose> registrant) {
          connectedNode.getLog().warn("Publisher failed to register: " + registrant);
        }
      });
    }

    // Publish the new Joint State
    final Publisher<sensor_msgs.JointState> publisherjoint = connectedNode.newPublisher("/iiwa/set_joint_state", sensor_msgs.JointState._TYPE);
    if (publisherjoint.hasSubscribers()) {
      sensor_msgs.JointState jointstate = publisherjoint.newMessage();
      publisherjoint.publish(jointstate);
      publisherjoint.addListener(new DefaultPublisherListener<sensor_msgs.JointState>() {
        @Override
        public void onMasterRegistrationFailure(Publisher<sensor_msgs.JointState> registrant) {
          connectedNode.getLog().warn("Publisher failed to register: " + registrant);
        }
      });
    }

      // Publish the new Ctrl State
      final Publisher<mindbot_msgs.CtrlState> publisherstate = connectedNode.newPublisher("/iiwa/set_ctrl_state", mindbot_msgs.CtrlState._TYPE);
      if (publisherstate.hasSubscribers()) {
        mindbot_msgs.CtrlState ctrlstate = publisherstate.newMessage();
        publisherstate.publish(ctrlstate);
        publisherstate.addListener(new DefaultPublisherListener<mindbot_msgs.CtrlState>() {
          @Override
          public void onMasterRegistrationFailure(Publisher<mindbot_msgs.CtrlState> registrant) {
            connectedNode.getLog().warn("Publisher failed to register: " + registrant);
          }
        });
      }
  }

  @Override
  public void onShutdown(Node node) {
    log.info("shutting down node" + node.getName());
    node.shutdown();
  }

  @Override
  public void onShutdownComplete(Node node) {

  }

  @Override
  public void onError(Node node, Throwable throwable) {

  }
}
