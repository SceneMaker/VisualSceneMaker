package mindbot.robot;

import org.apache.commons.logging.Log;
import org.ros.exception.RemoteException;
import org.ros.exception.RosRuntimeException;
import org.ros.exception.ServiceNotFoundException;
import org.ros.namespace.GraphName;
import org.ros.node.AbstractNodeMain;
import org.ros.node.ConnectedNode;
import org.ros.node.service.ServiceClient;
import org.ros.node.service.ServiceResponseListener;


public class MindbotClient extends AbstractNodeMain {
    @Override
    public GraphName getDefaultNodeName() {
        return GraphName.of("rosjava/MindbotClient");
    }
    ServiceClient<mindbot_msgs.SetPoseRequest, mindbot_msgs.SetPoseResponse> serviceClient;
    private Log log;

    public void onStart(final ConnectedNode connectedNode) {
        this.log = connectedNode.getLog();
        setUpClient(connectedNode);
    }

    public void setUpClient(ConnectedNode connectedNode) {
        try {
            serviceClient = connectedNode.newServiceClient("/iiwa/set_tcp_target", mindbot_msgs.SetPose._TYPE);
        } catch (ServiceNotFoundException e) {
            throw new RosRuntimeException(e);
        }
    }

    public void callClient() {
        mindbot_msgs.SetPoseRequest request = serviceClient.newMessage();
        geometry_msgs.Pose pose = request.getPose();
        pose.getPosition().setX(3);
        pose.getPosition().setY(1);
        pose.getPosition().setZ(1);
        pose.getOrientation().setW(1);
        pose.getOrientation().setX(1);
        pose.getOrientation().setY(1);
        pose.getOrientation().setZ(1);
        request.setPose(pose);
        serviceClient.call(request, new ServiceResponseListener<mindbot_msgs.SetPoseResponse>() {
            @Override
            public void onSuccess(mindbot_msgs.SetPoseResponse response) {
                log.info("The response is: " +response.getMessage());
            }

            @Override
            public void onFailure(RemoteException e) {
                throw new RosRuntimeException(e);
            }
        });
    }
}
