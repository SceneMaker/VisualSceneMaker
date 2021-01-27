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


public class MindbotServiceRequester extends AbstractNodeMain {
    @Override
    public GraphName getDefaultNodeName() {
        return GraphName.of("rosjava/MindbotClient");
    }

    private ServiceClient<mindbot_msgs.SetPoseRequest, mindbot_msgs.SetPoseResponse> _setTcpTargetService;
    // TODO -- Add here the other service clients
    // ...

    private Log log;

    public void onStart(final ConnectedNode connectedNode) {
        this.log = connectedNode.getLog();
        setUpClient(connectedNode);
    }

    public void setUpClient(ConnectedNode connectedNode) {
        try {
            this.log.info("Setting Up Client!!!!!") ;
            _setTcpTargetService = connectedNode.newServiceClient("/iiwa/set_tcp_target", mindbot_msgs.SetPose._TYPE);
        } catch (ServiceNotFoundException e) {
            throw new RosRuntimeException(e);
        }
    }


    public void setTcpTarget(float x, float y, float z, float or_w, float or_x, float or_y, float or_z) {
        mindbot_msgs.SetPoseRequest request = _setTcpTargetService.newMessage();

        geometry_msgs.Pose pose = request.getPose();
        pose.getPosition().setX(x);
        pose.getPosition().setY(y);
        pose.getPosition().setZ(z);
        pose.getOrientation().setW(or_w);
        pose.getOrientation().setX(or_x);
        pose.getOrientation().setY(or_y);
        pose.getOrientation().setZ(or_z);

        request.setPose(pose);
        _setTcpTargetService.call(request, new ServiceResponseListener<mindbot_msgs.SetPoseResponse>() {
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

    // TODO
    // Add the remaining services
    // ...
    /*
    /iiwa/set_joint_target           (mindbot_msgs::SetJointState)
    DONE /iiwa/set_tcp_target             (mindbot_msgs::SetPose)
    /iiwa/set_max_tcp_velocity       (mindbot_msgs::SetVector3)
    /iiwa/set_max_tcp_acceleration   (mindbot_msgs::SetVector3)
    /iiwa/set_ctrl_state             (cob_srvs::SetString)
    /iiwa/set_ctrl_mode              (cob_srvs::SetString)
    /iiwa/set_min_clearance          (cob_srvs::SetFloat)
    */

}
