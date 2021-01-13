// Auto-generated. Do not edit!

// (in-package mindbot_msgs.msg)


"use strict";

const _serializer = _ros_msg_utils.Serialize;
const _arraySerializer = _serializer.Array;
const _deserializer = _ros_msg_utils.Deserialize;
const _arrayDeserializer = _deserializer.Array;
const _finder = _ros_msg_utils.Find;
const _getByteLength = _ros_msg_utils.getByteLength;

//-----------------------------------------------------------

class CtrlState {
  constructor(initObj={}) {
    if (initObj === null) {
      // initObj === null is a special case for deserialization where we don't initialize fields
      this.ctrl_state = null;
    }
    else {
      if (initObj.hasOwnProperty('ctrl_state')) {
        this.ctrl_state = initObj.ctrl_state
      }
      else {
        this.ctrl_state = 0;
      }
    }
  }

  static serialize(obj, buffer, bufferOffset) {
    // Serializes a message object of type CtrlState
    // Serialize message field [ctrl_state]
    bufferOffset = _serializer.uint8(obj.ctrl_state, buffer, bufferOffset);
    return bufferOffset;
  }

  static deserialize(buffer, bufferOffset=[0]) {
    //deserializes a message object of type CtrlState
    let len;
    let data = new CtrlState(null);
    // Deserialize message field [ctrl_state]
    data.ctrl_state = _deserializer.uint8(buffer, bufferOffset);
    return data;
  }

  static getMessageSize(object) {
    return 1;
  }

  static datatype() {
    // Returns string type for a message object
    return 'mindbot_msgs/CtrlState';
  }

  static md5sum() {
    //Returns md5sum for a message object
    return '9bd2ce98651f7da53d3394dd6a9978bc';
  }

  static messageDefinition() {
    // Returns full string definition for message
    return `
    #CtrlState message
    
    #List of the available control states
    uint8 OFF = 0
    uint8 ON = 1
    uint8 ERROR = 2
    
    #Assigned control state
    uint8 ctrl_state
    `;
  }

  static Resolve(msg) {
    // deep-construct a valid message object instance of whatever was passed in
    if (typeof msg !== 'object' || msg === null) {
      msg = {};
    }
    const resolved = new CtrlState(null);
    if (msg.ctrl_state !== undefined) {
      resolved.ctrl_state = msg.ctrl_state;
    }
    else {
      resolved.ctrl_state = 0
    }

    return resolved;
    }
};

// Constants for message
CtrlState.Constants = {
  OFF: 0,
  ON: 1,
  ERROR: 2,
}

module.exports = CtrlState;
