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
    // CALLED -> EXECUTING -> ABORTED
    // CALLED -> EXECUTING -> DONE
    public enum CallState {
        CALLED,     // The remote ROS service has been called
        FAILURE,    // The remote ROS service answered FAILURE. Will not be executed. Don't wait for it.
        // TODO -- ad an UNSUCCESS state
        EXECUTING,    // The remote ROS service answered SUCCESS. Action is in execution on the ROS side. Expect an action_done call when finished.
        ABORTED,    // The remote ROS called back the action_done service to inform that the call couldn't execute properly.
        DONE        // The remote ROS called back the action_done to inform that the call was executed successfully.
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
    private ServiceClient<mindbot_msgs.SetGripperActionRequest, mindbot_msgs.SetGripperActionResponse> _setGripperActionService;
    private ServiceClient<mindbot_msgs.SetDetectionRequest, mindbot_msgs.SetDetectionResponse > _setDetectionService;

    private MindbotRobotFeedback mRobotFeedback;

    public MindbotServiceRequester(MindbotRobotFeedback feedback) {
        mRobotFeedback = feedback ;
    }

    @Override
    public void onStart(final ConnectedNode connectedNode) {
        setUpClient(connectedNode);
    }

    /** Will be set to true when the object Setup is done. */
    private boolean _setupDone = false ;

    /** An incremental counter to determine a local actionID for each call. */
    private static int _actionCounter = 0;

    /** Maps the local actionIDs to their call state.*/
    private final HashMap<Integer, CallState> actionsState = new HashMap<>();

    /** Maps the id that ROS generated for a call (rosID) to the local actionID. */
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
            _setTcpTargetService = connectedNode.newServiceClient("/mindbot/robot/set_tcp_target", mindbot_msgs.SetPose._TYPE);
            _setJointTargetService = connectedNode.newServiceClient("/mindbot/robot/set_joint_target", mindbot_msgs.SetJointState._TYPE);
            _setMaxTcpVelocityService = connectedNode.newServiceClient("/mindbot/robot/set_max_tcp_velocity", mindbot_msgs.SetVector3._TYPE);
            _setMaxTcpAccelerationService = connectedNode.newServiceClient("/mindbot/robot/set_max_tcp_acceleration", mindbot_msgs.SetVector3._TYPE);
            _setCtrlStateService = connectedNode.newServiceClient("/mindbot/robot/set_ctrl_state", mindbot_msgs.SetCtrlState._TYPE);
            _setCtrlModeService = connectedNode.newServiceClient("/mindbot/robot/set_ctrl_mode", mindbot_msgs.SetCtrlMode._TYPE);
            _setMinClearanceService = connectedNode.newServiceClient("/mindbot/robot/set_min_clearance", mindbot_msgs.SetFloat._TYPE);
            _setGripperActionService = connectedNode.newServiceClient("/mindbot/robot/set_gripper_action", mindbot_msgs.SetGripperAction._TYPE);
            _setDetectionService = connectedNode.newServiceClient("/mindbot/robot/set_detection", mindbot_msgs.SetDetection._TYPE) ;

            //
            // Instantiate the listening service, receiving the result of the calls.
            connectedNode.newServiceServer(
                    "/mindbot/robot/action_done",
                    mindbot_msgs.VSMActionDone._TYPE,
                    new ActionDoneResponseBuilder());

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
            // TODO -- here, if we wait for too long, it probably means that the robot is disconnected
            // TODO -- We should inform the user and abort the project.
            synchronized (actionsState) {
                s = actionsState.get(actionID);
                if (s == CallState.DONE || s == CallState.FAILURE || s == CallState.ABORTED) {
                    break;
                } else {
                    // Here, the call is either just CALLED or SUCCESS. We have to wait.
                    try {
                        actionsState.wait(1000);
                        mRobotFeedback.logWarning("Still waiting for 'action_done' for localID " + actionID);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            }

        }

        // Delete the localID and rosID from the maps
        synchronized (actionsState) {
            // (I know, it is linear complexity, but we don't have BiMaps and anyway the size is always limited.)
            for (int ros_id : rosToActionID.keySet()) {
                int act_id = rosToActionID.get(ros_id);
                if (act_id == actionID) {
                    rosToActionID.remove(ros_id) ;
                }
            }

            //
            // remove the IDs from the actionMap
            actionsState.remove(actionID);

        }


        return s ;
    }

    /**
     * This is the code building the answer to the messages receives on the "action_done" service.
     */
    private class ActionDoneResponseBuilder implements ServiceResponseBuilder<mindbot_msgs.VSMActionDoneRequest, mindbot_msgs.VSMActionDoneResponse> {

        /** This will be invoked every time the local "action_done" service is invoked.
         * Here, the goal is to:
         * * retrieve the rosID, i.e., the action ID from the ROS side.
         * * set the status of the action associated to the rosID to either DONE or ABORTED
         * * trigger the thread waiting for the conclusion of the action.
         *
         * @param request message, callid (aka rosID), result
         * @param response success
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

                    mRobotFeedback.setActionState(s.toString());
                    mRobotFeedback.setActionMessage(message);
                }
                // if the is was not in the map, it is possible that VSM re-started when a robot acton was still executing
            }

            // Set the response result.
            response.setSuccess(true);
        }
    }

    /** Centralized parametric Listener that can be used to intercept the answer from any robot service.
     * If the expect_action_done_message in the constructor is true,
     * the responding service must provide a field named `action_id` of type int32.
     * The constructor will insert the actionID in the `actionState` table with state `CALLED`.
     * The onSuccess() and onFailure() will update the state accordingly.
     */
    class MindBotResponseListener<T extends org.ros.internal.message.Message> implements ServiceResponseListener<T> {

        /** This is identifier of the VSM Action.
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

                    mRobotFeedback.setActionState(CallState.CALLED.toString());
                    mRobotFeedback.setActionMessage("");
                }
            }

        }

        int getActionID() {
            return this.vsmActionID ;
        }

        /** This will be invoked as a (positive) result when th execution of a ROS service has succeeded.
         * We update the state of the running action and memorize the association between the action localID and rosID.
         */
        @Override
        public void onSuccess(Message response) {

            boolean success = response.toRawMessage().getBool("success") ;
            String message = response.toRawMessage().getString("message") ;
            // TODO -- test this and gracefully fail if false


            if(this.vsmActionID != -1) {
                int rosActionID = response.toRawMessage().getInt32("action_id");
                synchronized (actionsState) {
                    actionsState.put(vsmActionID, CallState.EXECUTING);
                    rosToActionID.put(rosActionID, vsmActionID);

                    mRobotFeedback.setActionState(CallState.EXECUTING.toString());

                }
            }

        }

        /** This is invoked by ROS in case of failure of the infrastructure, i.e., if the call couldn't be executed.
         */
        @Override
        public void onFailure(RemoteException e) {

            if(this.vsmActionID != -1) {

                synchronized (actionsState) {
                    actionsState.put(vsmActionID, CallState.FAILURE);

                    mRobotFeedback.setActionState(CallState.FAILURE.toString());
                }
            }

            throw new RosRuntimeException(e);

        }

    }


    /**     /mindbot/robot/set_joint_target           (mindbot_msgs::SetJointState)
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

    /** /mindbot/robot/set_tcp_target             (mindbot_msgs::SetPose)
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


    /** /mindbot/robot/set_max_tcp_velocity       (mindbot_msgs::SetVector3)
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

    /** /mindbot/robot/set_max_tcp_acceleration   (mindbot_msgs::SetVector3)
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


    /** /mindbot/robot/set_ctrl_state             (cob_srvs::SetString)
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

    /** /mindbot/robot/set_ctrl_mode              (cob_srvs::SetString)
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

    /**     /mindbot/robot/set_min_clearance           (mindbot_msgs::SetFloat)
     *
     * @param min_clearance A float with the minimum accepted distance between robot and operator.
     */
    public void setMinClearanceService(float min_clearance) {
        mindbot_msgs.SetFloatRequest request = _setMinClearanceService.newMessage();

        request.setData(min_clearance);

        MindBotResponseListener<SetFloatResponse> listener = new MindBotResponseListener<SetFloatResponse>(false);
        _setMinClearanceService.call(request, listener);
    }


    /**     /mindbot/robot/set_gripper_action        (mindbot_msgs::SetGripperAction)
     *
     * @param position 0 (all open) to 255 (all closed, but unlikely perfectly 0mm)
     * @param velocity 0 to 255 (ca. 150mm/sec)
     * @param force 0 to 255 (ca. 5Kg)
     */
    public void setGripperAction(int position, int velocity, int force) {
        SetGripperActionRequest request = _setGripperActionService.newMessage();

        request.setPosition(position);
        request.setVelocity(velocity);
        request.setForce(force);

        MindBotResponseListener<SetGripperActionResponse> listener = new MindBotResponseListener<>(true) ;
        _setGripperActionService.call(request, listener);
    }

    /** /mindbot/robot/set_detection
     * Service type is mindbot_msgs::SetDetection
     *
     * @param obj_name The nam of the object to be detected.
     */
    public void setVisualDetection(String obj_name) {
        SetDetectionRequest request = _setDetectionService.newMessage() ;

        request.setObjectName(obj_name);

        MindBotResponseListener<SetDetectionResponse> listener = new MindBotResponseListener<>(true) ;
        _setDetectionService.call(request, listener) ;
    }

}
