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

class CtrlMode {
  constructor(initObj={}) {
    if (initObj === null) {
      // initObj === null is a special case for deserialization where we don't initialize fields
      this.ctrl_mode = null;
    }
    else {
      if (initObj.hasOwnProperty('ctrl_mode')) {
        this.ctrl_mode = initObj.ctrl_mode
      }
      else {
        this.ctrl_mode = 0;
      }
    }
  }

  static serialize(obj, buffer, bufferOffset) {
    // Serializes a message object of type CtrlMode
    // Serialize message field [ctrl_mode]
    bufferOffset = _serializer.uint8(obj.ctrl_mode, buffer, bufferOffset);
    return bufferOffset;
  }

  static deserialize(buffer, bufferOffset=[0]) {
    //deserializes a message object of type CtrlMode
    let len;
    let data = new CtrlMode(null);
    // Deserialize message field [ctrl_mode]
    data.ctrl_mode = _deserializer.uint8(buffer, bufferOffset);
    return data;
  }

  static getMessageSize(object) {
    return 1;
  }

  static datatype() {
    // Returns string type for a message object
    return 'mindbot_msgs/CtrlMode';
  }

  static md5sum() {
    //Returns md5sum for a message object
    return 'e5e929f57b05b5ae4f0748d62736ba48';
  }

  static messageDefinition() {
    // Returns full string definition for message
    return `
    #CtrlMode message
    
    #List of the available control modes
    uint8 MODE0 = 0
    uint8 MODE1 = 1
    uint8 MODE2 = 2
    
    #Assigned control mode
    uint8 ctrl_mode
    `;
  }

  static Resolve(msg) {
    // deep-construct a valid message object instance of whatever was passed in
    if (typeof msg !== 'object' || msg === null) {
      msg = {};
    }
    const resolved = new CtrlMode(null);
    if (msg.ctrl_mode !== undefined) {
      resolved.ctrl_mode = msg.ctrl_mode;
    }
    else {
      resolved.ctrl_mode = 0
    }

    return resolved;
    }
};

// Constants for message
CtrlMode.Constants = {
  MODE0: 0,
  MODE1: 1,
  MODE2: 2,
}

module.exports = CtrlMode;
