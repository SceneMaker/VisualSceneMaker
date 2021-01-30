package de.dfki.vsm.xtension.mindbotrobot;

import org.apache.commons.logging.Log;
import org.ros.exception.RemoteException;
import org.ros.exception.RosRuntimeException;
import org.ros.exception.ServiceNotFoundException;
import org.ros.namespace.GraphName;
import org.ros.node.AbstractNodeMain;
import org.ros.node.ConnectedNode;
import org.ros.node.service.ServiceClient;
import org.ros.node.service.ServiceResponseListener;

import java.util.List;


public class MindbotServiceRequester extends AbstractNodeMain {
    @Override
    public GraphName getDefaultNodeName() {
        return GraphName.of("rosjava/MindbotClient");
    }

    private ServiceClient<mindbot_msgs.SetPoseRequest, mindbot_msgs.SetPoseResponse> _setTcpTargetService;
    private ServiceClient<mindbot_msgs.SetJointStateRequest, mindbot_msgs.SetJointStateResponse> _setJointTargetService;
    private ServiceClient<mindbot_msgs.SetVector3Request, mindbot_msgs.SetVector3Response> _setMaxTcpVelocityService;
    private ServiceClient<mindbot_msgs.SetVector3Request, mindbot_msgs.SetVector3Response> _setMaxTcpAccelerationService;
    private ServiceClient<mindbot_msgs.SetCtrlStateRequest, mindbot_msgs.SetCtrlStateResponse> _setCtrlStateService;
    private ServiceClient<mindbot_msgs.SetCtrlModeRequest, mindbot_msgs.SetCtrlModeResponse> _setCtrlModeService;
    private ServiceClient<mindbot_msgs.SetFloatRequest, mindbot_msgs.SetFloatResponse> _setMinClearanceService;

    private Log log;

    public void onStart(final ConnectedNode connectedNode) {
        this.log = connectedNode.getLog();
        setUpClient(connectedNode);
    }

    public boolean isSetupDone() {
        return _setupDone;
    }

    private boolean _setupDone = false ;

    public void setUpClient(ConnectedNode connectedNode) {
        try {
            this.log.info("Setting Up the ServiceRequester!!!!!") ;
            _setTcpTargetService = connectedNode.newServiceClient("/iiwa/set_tcp_target", mindbot_msgs.SetPose._TYPE);
            _setJointTargetService = connectedNode.newServiceClient("/iiwa/set_joint_target", mindbot_msgs.SetJointState._TYPE);
            _setMaxTcpVelocityService = connectedNode.newServiceClient("/iiwa/set_max_tcp_velocity", mindbot_msgs.SetVector3._TYPE);
            _setMaxTcpAccelerationService = connectedNode.newServiceClient("/iiwa/set_max_tcp_acceleration", mindbot_msgs.SetVector3._TYPE);
            _setCtrlStateService = connectedNode.newServiceClient("/iiwa/set_ctrl_state", mindbot_msgs.SetCtrlState._TYPE);
            _setCtrlModeService = connectedNode.newServiceClient("/iiwa/set_ctrl_mode", mindbot_msgs.SetCtrlMode._TYPE);
            _setMinClearanceService = connectedNode.newServiceClient("/iiwa/set_min_clearance", mindbot_msgs.SetFloat._TYPE);
        } catch (ServiceNotFoundException e) {
            throw new RosRuntimeException(e);
        }

        _setupDone = true ;
    }


    /**     /iiwa/set_joint_target           (mindbot_msgs::SetJointState)
     *
     * @param joint_names The name of the joints to modify.
     * @param p The position (actually the rotation) of each joint, in the same order of the names.
     * @param v The velocity of each joint, in the same order of the names.
     * @param e The effort of each joint, in the same order of the names.
     */
    public void setJointTarget(List<String> joint_names, double[] p, double[] v, double[] e) {
        mindbot_msgs.SetJointStateRequest request = _setJointTargetService.newMessage();

        sensor_msgs.JointState jointState = request.getPoint();
        jointState.setName(joint_names);
        jointState.setPosition(p);
        jointState.setVelocity(v);
        jointState.setEffort(e);

        request.setPoint(jointState);
        _setJointTargetService.call(request, new ServiceResponseListener<mindbot_msgs.SetJointStateResponse>() {
            @Override
            public void onSuccess(mindbot_msgs.SetJointStateResponse response) {
                log.info("The response is: " +response.getMessage());
            }

            @Override
            public void onFailure(RemoteException e) {
                throw new RosRuntimeException(e);
            }
        });
    }

    /** /iiwa/set_tcp_target             (mindbot_msgs::SetPose)
     *
     * @param x
     * @param y
     * @param z
     * @param or_w
     * @param or_x
     * @param or_y
     * @param or_z
     */
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


    /** /iiwa/set_max_tcp_velocity       (mindbot_msgs::SetVector3)
     *
     * @param x
     * @param y
     * @param z
     */
     public void setMaxTcpVelocity(double x, double y, double z) {
        mindbot_msgs.SetVector3Request request = _setMaxTcpVelocityService.newMessage();

        geometry_msgs.Vector3 maxTcpVelocity = request.getData();
        maxTcpVelocity.setX(x);
        maxTcpVelocity.setY(y);
        maxTcpVelocity.setZ(z);

        request.setData(maxTcpVelocity);
        _setMaxTcpVelocityService.call(request, new ServiceResponseListener<mindbot_msgs.SetVector3Response>() {
            @Override
            public void onSuccess(mindbot_msgs.SetVector3Response response) {
                log.info("The response is: " +response.getMessage());
            }

            @Override
            public void onFailure(RemoteException e) {
                throw new RosRuntimeException(e);
            }
        });
    }

    /** /iiwa/set_max_tcp_acceleration   (mindbot_msgs::SetVector3)
     *
     * @param x
     * @param y
     * @param z
     */
     public void setMaxTcpAcceleration(double x, double y, double z) {
        mindbot_msgs.SetVector3Request request = _setMaxTcpAccelerationService.newMessage();

        geometry_msgs.Vector3 maxTcpAcceleration = request.getData();
        maxTcpAcceleration.setX(x);
        maxTcpAcceleration.setY(y);
        maxTcpAcceleration.setZ(z);

        request.setData(maxTcpAcceleration);
        _setMaxTcpAccelerationService.call(request, new ServiceResponseListener<mindbot_msgs.SetVector3Response>() {
            @Override
            public void onSuccess(mindbot_msgs.SetVector3Response response) {
                log.info("The response is: " +response.getMessage());
            }

            @Override
            public void onFailure(RemoteException e) {
                throw new RosRuntimeException(e);
            }
        });
    }


    /** /iiwa/set_ctrl_state             (cob_srvs::SetString)
     *
     * @param state
     */
     public void setCtrlState(byte state) {
        mindbot_msgs.SetCtrlStateRequest request = _setCtrlStateService.newMessage();

        mindbot_msgs.CtrlState ctrlState = request.getCtrlState();
        ctrlState.setCtrlState(state);

        request.setCtrlState(ctrlState);
        _setCtrlStateService.call(request, new ServiceResponseListener<mindbot_msgs.SetCtrlStateResponse>() {
            @Override
            public void onSuccess(mindbot_msgs.SetCtrlStateResponse response) {
                log.info("The response is: " +response.getMessage());
            }

            @Override
            public void onFailure(RemoteException e) {
                throw new RosRuntimeException(e);
            }
        });
    }

    /** /iiwa/set_ctrl_mode              (cob_srvs::SetString)
     *
     * @param mode
     */
     public void setCtrlMode(byte mode) {
        mindbot_msgs.SetCtrlModeRequest request = _setCtrlModeService.newMessage();

        mindbot_msgs.CtrlMode ctrlMode = request.getCtrlMode();
        ctrlMode.setCtrlMode(mode);

        request.setCtrlMode(ctrlMode);
        _setCtrlModeService.call(request, new ServiceResponseListener<mindbot_msgs.SetCtrlModeResponse>() {
            @Override
            public void onSuccess(mindbot_msgs.SetCtrlModeResponse response) {
                log.info("The response is: " +response.getMessage());
            }

            @Override
            public void onFailure(RemoteException e) {
                throw new RosRuntimeException(e);
            }
        });
    }

    /**     /iiwa/set_min_clearance           (mindbot_msgs::SetFloat)
     *
     * @param min_clearance A float with the minimum accepted distance between robot and operator.
     */
    public void setMinClearanceService(float min_clearance) {
        mindbot_msgs.SetFloatRequest request = _setMinClearanceService.newMessage();

        request.setData(min_clearance);
        _setMinClearanceService.call(request, new ServiceResponseListener<mindbot_msgs.SetFloatResponse>() {
            @Override
            public void onSuccess(mindbot_msgs.SetFloatResponse response) {
                log.info("The response is: " +response.getSuccess());
            }

            @Override
            public void onFailure(RemoteException e) {
                throw new RosRuntimeException(e);
            }
        });
    }

}
