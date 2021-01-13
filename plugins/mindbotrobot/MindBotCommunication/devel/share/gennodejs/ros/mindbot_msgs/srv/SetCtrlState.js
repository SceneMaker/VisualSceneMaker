// Auto-generated. Do not edit!

// (in-package mindbot_msgs.srv)


"use strict";

const _serializer = _ros_msg_utils.Serialize;
const _arraySerializer = _serializer.Array;
const _deserializer = _ros_msg_utils.Deserialize;
const _arrayDeserializer = _deserializer.Array;
const _finder = _ros_msg_utils.Find;
const _getByteLength = _ros_msg_utils.getByteLength;
let CtrlState = require('../msg/CtrlState.js');

//-----------------------------------------------------------


//-----------------------------------------------------------

class SetCtrlStateRequest {
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
        this.ctrl_state = new CtrlState();
      }
    }
  }

  static serialize(obj, buffer, bufferOffset) {
    // Serializes a message object of type SetCtrlStateRequest
    // Serialize message field [ctrl_state]
    bufferOffset = CtrlState.serialize(obj.ctrl_state, buffer, bufferOffset);
    return bufferOffset;
  }

  static deserialize(buffer, bufferOffset=[0]) {
    //deserializes a message object of type SetCtrlStateRequest
    let len;
    let data = new SetCtrlStateRequest(null);
    // Deserialize message field [ctrl_state]
    data.ctrl_state = CtrlState.deserialize(buffer, bufferOffset);
    return data;
  }

  static getMessageSize(object) {
    return 1;
  }

  static datatype() {
    // Returns string type for a service object
    return 'mindbot_msgs/SetCtrlStateRequest';
  }

  static md5sum() {
    //Returns md5sum for a message object
    return '3c11e4fe52b5a04acc1f4850c19306fe';
  }

  static messageDefinition() {
    // Returns full string definition for message
    return `
    #SetCtrlState service
    
    #REQUEST
    mindbot_msgs/CtrlState ctrl_state
    
    
    ================================================================================
    MSG: mindbot_msgs/CtrlState
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
    const resolved = new SetCtrlStateRequest(null);
    if (msg.ctrl_state !== undefined) {
      resolved.ctrl_state = CtrlState.Resolve(msg.ctrl_state)
    }
    else {
      resolved.ctrl_state = new CtrlState()
    }

    return resolved;
    }
};

class SetCtrlStateResponse {
  constructor(initObj={}) {
    if (initObj === null) {
      // initObj === null is a special case for deserialization where we don't initialize fields
      this.success = null;
      this.message = null;
    }
    else {
      if (initObj.hasOwnProperty('success')) {
        this.success = initObj.success
      }
      else {
        this.success = false;
      }
      if (initObj.hasOwnProperty('message')) {
        this.message = initObj.message
      }
      else {
        this.message = '';
      }
    }
  }

  static serialize(obj, buffer, bufferOffset) {
    // Serializes a message object of type SetCtrlStateResponse
    // Serialize message field [success]
    bufferOffset = _serializer.bool(obj.success, buffer, bufferOffset);
    // Serialize message field [message]
    bufferOffset = _serializer.string(obj.message, buffer, bufferOffset);
    return bufferOffset;
  }

  static deserialize(buffer, bufferOffset=[0]) {
    //deserializes a message object of type SetCtrlStateResponse
    let len;
    let data = new SetCtrlStateResponse(null);
    // Deserialize message field [success]
    data.success = _deserializer.bool(buffer, bufferOffset);
    // Deserialize message field [message]
    data.message = _deserializer.string(buffer, bufferOffset);
    return data;
  }

  static getMessageSize(object) {
    let length = 0;
    length += object.message.length;
    return length + 5;
  }

  static datatype() {
    // Returns string type for a service object
    return 'mindbot_msgs/SetCtrlStateResponse';
  }

  static md5sum() {
    //Returns md5sum for a message object
    return '937c9679a518e3a18d831e57125ea522';
  }

  static messageDefinition() {
    // Returns full string definition for message
    return `
    
    #RESPONSE
    bool success
    string message
    
    
    `;
  }

  static Resolve(msg) {
    // deep-construct a valid message object instance of whatever was passed in
    if (typeof msg !== 'object' || msg === null) {
      msg = {};
    }
    const resolved = new SetCtrlStateResponse(null);
    if (msg.success !== undefined) {
      resolved.success = msg.success;
    }
    else {
      resolved.success = false
    }

    if (msg.message !== undefined) {
      resolved.message = msg.message;
    }
    else {
      resolved.message = ''
    }

    return resolved;
    }
};

module.exports = {
  Request: SetCtrlStateRequest,
  Response: SetCtrlStateResponse,
  md5sum() { return '60923bc5e5c8d30acc73ee0d6e56f3ab'; },
  datatype() { return 'mindbot_msgs/SetCtrlState'; }
};
