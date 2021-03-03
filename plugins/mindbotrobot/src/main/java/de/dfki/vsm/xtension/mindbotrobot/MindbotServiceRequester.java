package de.dfki.vsm.xtension.mindbotrobot;

import mindbot_msgs.*;
import org.ros.exception.RemoteException;
import org.ros.exception.RosRuntimeException;
import org.ros.exception.ServiceNotFoundException;
import org.ros.internal.message.Message;
import org.ros.namespace.GraphName;
import org.ros.node.AbstractNodeMain;
import org.ros.node.ConnectedNode;
import org.ros.node.service.ServiceClient;
import org.ros.node.service.ServiceResponseBuilder;
import org.ros.node.service.ServiceResponseListener;

import java.util.List;
import java.util.HashMap;


public class MindbotServiceRequester extends AbstractNodeMain {

    // Possible state paths:
    // CALLED -> FAILURE
    // CALLED -> SUCCESS -> ABORTED
    // CALLED -> SUCCESS -> DONE
    public enum CallState {
        CALLED,     // The remote ROS service has been called
        FAILURE,    // The remote ROS service answered FAILURE (will not be executed. Don't wait for it)
        SUCCESS,    // The remote ROS service answered SUCCESS (still being in execution on the ROS side)
        ABORTED,    // The remote ROS called back our service to inform that the call couldn't execute properly
        DONE        // The remote ROS called back our service to inform that the call was executed successfully
    }


    @Override
    public GraphName getDefaultNodeName() {
        return GraphName.of("mindbot/vsm/RobotServiceRequester");
    }

    private ServiceClient<mindbot_msgs.SetPoseRequest, mindbot_msgs.SetPoseResponse> _setTcpTargetService;
    private ServiceClient<mindbot_msgs.SetJointStateRequest, mindbot_msgs.SetJointStateResponse> _setJointTargetService;
    private ServiceClient<mindbot_msgs.SetVector3Request, mindbot_msgs.SetVector3Response> _setMaxTcpVelocityService;
    private ServiceClient<mindbot_msgs.SetVector3Request, mindbot_msgs.SetVector3Response> _setMaxTcpAccelerationService;
    private ServiceClient<mindbot_msgs.SetCtrlStateRequest, mindbot_msgs.SetCtrlStateResponse> _setCtrlStateService;
    private ServiceClient<mindbot_msgs.SetCtrlModeRequest, mindbot_msgs.SetCtrlModeResponse> _setCtrlModeService;
    private ServiceClient<mindbot_msgs.SetFloatRequest, mindbot_msgs.SetFloatResponse> _setMinClearanceService;

    @Override
    public void onStart(final ConnectedNode connectedNode) {
        setUpClient(connectedNode);
    }

    /** Will be set to true when the object Setup is done. */
    private boolean _setupDone = false ;

    /** An incremental counter to determine a local actionID for each call. */
    private static int _actionCounter = 0;

    /** Maps the call IDs to their call state.*/
    private final HashMap<Integer, CallState> actionsState = new HashMap<>();

    /** Maps the id that ROS generated for a call to the local actionID. */
    private final  HashMap<Integer, Integer> rosToActionID = new HashMap<>() ;

    /** Check for setup state.
     *
     * @return true if the ROS onStart terminated without exceptions.
     */
    public boolean isSetupDone() {
        return (_setupDone);
    }

    private void setUpClient(ConnectedNode connectedNode) {
        try {
            _setTcpTargetService = connectedNode.newServiceClient("/iiwa/set_tcp_target", mindbot_msgs.SetPose._TYPE);
            _setJointTargetService = connectedNode.newServiceClient("/iiwa/set_joint_target", mindbot_msgs.SetJointState._TYPE);
            _setMaxTcpVelocityService = connectedNode.newServiceClient("/iiwa/set_max_tcp_velocity", mindbot_msgs.SetVector3._TYPE);
            _setMaxTcpAccelerationService = connectedNode.newServiceClient("/iiwa/set_max_tcp_acceleration", mindbot_msgs.SetVector3._TYPE);
            _setCtrlStateService = connectedNode.newServiceClient("/iiwa/set_ctrl_state", mindbot_msgs.SetCtrlState._TYPE);
            _setCtrlModeService = connectedNode.newServiceClient("/iiwa/set_ctrl_mode", mindbot_msgs.SetCtrlMode._TYPE);
            _setMinClearanceService = connectedNode.newServiceClient("/iiwa/set_min_clearance", mindbot_msgs.SetFloat._TYPE);

            //
            // Instantiate the listening service, receiving the result of the calls.
            connectedNode.newServiceServer("/mindbot/robot/action_done", mindbot_msgs.VSMActionDone._TYPE,
                    new ServiceResponseBuilder<mindbot_msgs.VSMActionDoneRequest, mindbot_msgs.VSMActionDoneResponse>() {

                        /** This will be invoked every time the local "action_done" service is invoked.
                         * Here, the goal is to set the status of the action call and trigger the waiting threads.
                         *
                         * @param request callid & resultid
                         * @param response message & success
                         */
                        @Override
                        public void build(mindbot_msgs.VSMActionDoneRequest request, mindbot_msgs.VSMActionDoneResponse response) {
                            int rosCallID = request.getCallID();
                            // the result ID: 0=ERROR, 1=OK
                            int result = request.getResult();
                            CallState s = (result == 1) ? CallState.DONE : CallState.ABORTED;

                            String message = request.getMessage() ;

                            synchronized (actionsState) {
                                if(rosToActionID.containsKey(rosCallID)) {
                                    // This will set the state of the action and notify threads waiting for the call.
                                    int actionID = rosToActionID.get(rosCallID);
                                    actionsState.put(actionID, s);
                                    actionsState.notifyAll();
                                }
                                // if the is was not in the map, it is üossible that VSM (re-started) when a robot acton was still executing
                            }

                            // Set the response result.
                            response.setSuccess(true);
                        }
                    }) ;

        } catch (ServiceNotFoundException e) {
            throw new RosRuntimeException(e);
        }

        _setupDone = true ;
    }

    /** Wait for an action until it is terminated.
     *
     * @param actionID
     * @return The last CallState registered
     */
    public CallState waitAction(int actionID) {

        // Will retain he last state for this action
        CallState s;

        // Loop until the action is being processed
        while (true) {
            synchronized (actionsState) {
                s = actionsState.get(actionID);
                if (s == CallState.DONE || s == CallState.FAILURE || s == CallState.ABORTED) {
                    break;
                } else {
                    // Here, the call is either just CALLED or SUCCESS. We have to wait.
                    try {
                        actionsState.wait();
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            }

        }


        // Remove the
        synchronized (actionsState) {
            //
            // remove the IDs from the actionMap
            actionsState.remove(actionID);

            // remove the ID from the ros map.
            // (I know, it is linear complexity, but we don't have BiMaps and anyway the size is always limited.)
            for (int ros_id : rosToActionID.keySet()) {
                int act_id = rosToActionID.get(ros_id);
                if (act_id == actionID) {
                    rosToActionID.remove(ros_id);
                    break;
                }
            }
        }

        return s ;
    }


    /** Centralized parametric Listener that can be used to intercept the answer from any service.
     * If the expect_action_done_message in the constructor is true,
     * the responding service must provide a field named `action_id` of type int32.
     * The constructor will insert the actionID in the `actionState` table with state `CALLED`.
     * The onSuccess() and onFailure() will update the state accordingly.
     */
    class MindBotResponseListener<T extends org.ros.internal.message.Message> implements ServiceResponseListener<T> {

        /** This is identificator of the VSM Action.
         * By default is -1.
         * However, if the listener must setup the waiting procedure, it will be initialized to a unique integer counter.
         */
        int vsmActionID = -1 ;

        public MindBotResponseListener(boolean expect_action_done_message) {

            // Generate a new local VSM ID for this action call
            if(expect_action_done_message) {
                this.vsmActionID = _actionCounter++;
                synchronized (actionsState) {
                    actionsState.put(this.vsmActionID, CallState.CALLED);
                }
            }

        }

        int getActionID() {
            return this.vsmActionID ;
        }

        @Override
        public void onSuccess(Message response) {

            boolean success = response.toRawMessage().getBool("success") ;
            // TODO -- test this and gracefully fail if false

            if(this.vsmActionID != -1) {
                int rosActionID = response.toRawMessage().getInt32("action_id");
                synchronized (actionsState) {
                    actionsState.put(vsmActionID, CallState.SUCCESS);
                    rosToActionID.put(rosActionID, vsmActionID);
                }
            }

        }

        @Override
        public void onFailure(RemoteException e) {

            if(this.vsmActionID != -1) {

                // This is invoked b ROS in can of failure of the infrastructure
                synchronized (actionsState) {
                    actionsState.put(vsmActionID, CallState.FAILURE);
                }
                throw new RosRuntimeException(e);
            }

        }
    }


    /**     /iiwa/set_joint_target           (mindbot_msgs::SetJointState)
     *
     * @param joint_names The name of the joints to modify.
     * @param p The position (actually the rotation in radians) of each joint, in the same order of the names.
     * @param v The velocity (rad/sec) of each joint, in the same order of the names.
     * @param e The effort (N/m) of each joint, in the same order of the names.
     */
    public int setJointTarget(List<String> joint_names, double[] p, double[] v, double[] e) {
        mindbot_msgs.SetJointStateRequest request = _setJointTargetService.newMessage();

        sensor_msgs.JointState jointState = request.getPoint();
        jointState.setName(joint_names);
        jointState.setPosition(p);
        jointState.setVelocity(v);
        jointState.setEffort(e);

        request.setPoint(jointState);

        MindBotResponseListener<SetJointStateResponse> listener = new MindBotResponseListener<SetJointStateResponse>(true);
        _setJointTargetService.call(request, listener);

        return listener.getActionID() ;
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
    public int setTcpTarget(float x, float y, float z, float or_w, float or_x, float or_y, float or_z) {
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

        MindBotResponseListener<SetPoseResponse> listener = new MindBotResponseListener<SetPoseResponse>(true);
        _setTcpTargetService.call(request, listener);

        return listener.getActionID() ;
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

        MindBotResponseListener<SetVector3Response> listener = new MindBotResponseListener<SetVector3Response>(false);
        _setMaxTcpVelocityService.call(request, listener);
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

        MindBotResponseListener<SetVector3Response> listener = new MindBotResponseListener<SetVector3Response>(false);
        _setMaxTcpAccelerationService.call(request, listener);
    }


    /** /iiwa/set_ctrl_state             (cob_srvs::SetString)
     *
     * @param state     OFF = 0
     *                  ON = 1
     *                  ERROR = 2
     */
    public void setCtrlState(byte state) {
        mindbot_msgs.SetCtrlStateRequest request = _setCtrlStateService.newMessage();

        mindbot_msgs.CtrlState ctrlState = request.getCtrlState();
        ctrlState.setCtrlState(state);

        request.setCtrlState(ctrlState);

        MindBotResponseListener<SetCtrlStateResponse> listener = new MindBotResponseListener<SetCtrlStateResponse>(false);
        _setCtrlStateService.call(request, listener);
    }

    /** /iiwa/set_ctrl_mode              (cob_srvs::SetString)
     *
     * @param mode      MODEO = 0
     *                  MODE1 = 1
     *                  MODE2 = 2
     */
    public void setCtrlMode(byte mode) {
        mindbot_msgs.SetCtrlModeRequest request = _setCtrlModeService.newMessage();

        mindbot_msgs.CtrlMode ctrlMode = request.getCtrlMode();
        ctrlMode.setCtrlMode(mode);

        request.setCtrlMode(ctrlMode);

        MindBotResponseListener<SetCtrlModeResponse> listener = new MindBotResponseListener<SetCtrlModeResponse>(false);
        _setCtrlModeService.call(request, listener);
    }

    /**     /iiwa/set_min_clearance           (mindbot_msgs::SetFloat)
     *
     * @param min_clearance A float with the minimum accepted distance between robot and operator.
     */
    public void setMinClearanceService(float min_clearance) {
        mindbot_msgs.SetFloatRequest request = _setMinClearanceService.newMessage();

        request.setData(min_clearance);

        MindBotResponseListener<SetFloatResponse> listener = new MindBotResponseListener<SetFloatResponse>(false);
        _setMinClearanceService.call(request, listener);
    }

}
