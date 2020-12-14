// Auto-generated. Do not edit!

// (in-package mindbot_msgs.srv)


"use strict";

const _serializer = _ros_msg_utils.Serialize;
const _arraySerializer = _serializer.Array;
const _deserializer = _ros_msg_utils.Deserialize;
const _arrayDeserializer = _deserializer.Array;
const _finder = _ros_msg_utils.Find;
const _getByteLength = _ros_msg_utils.getByteLength;
let CtrlMode = require('../msg/CtrlMode.js');

//-----------------------------------------------------------


//-----------------------------------------------------------

class SetCtrlModeRequest {
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
        this.ctrl_mode = new CtrlMode();
      }
    }
  }

  static serialize(obj, buffer, bufferOffset) {
    // Serializes a message object of type SetCtrlModeRequest
    // Serialize message field [ctrl_mode]
    bufferOffset = CtrlMode.serialize(obj.ctrl_mode, buffer, bufferOffset);
    return bufferOffset;
  }

  static deserialize(buffer, bufferOffset=[0]) {
    //deserializes a message object of type SetCtrlModeRequest
    let len;
    let data = new SetCtrlModeRequest(null);
    // Deserialize message field [ctrl_mode]
    data.ctrl_mode = CtrlMode.deserialize(buffer, bufferOffset);
    return data;
  }

  static getMessageSize(object) {
    return 1;
  }

  static datatype() {
    // Returns string type for a service object
    return 'mindbot_msgs/SetCtrlModeRequest';
  }

  static md5sum() {
    //Returns md5sum for a message object
    return '45573c06451852ab4829ec27b7af4563';
  }

  static messageDefinition() {
    // Returns full string definition for message
    return `
    #SetCtrlMode service
    
    #REQUEST
    mindbot_msgs/CtrlMode ctrl_mode
    
    
    ================================================================================
    MSG: mindbot_msgs/CtrlMode
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
    const resolved = new SetCtrlModeRequest(null);
    if (msg.ctrl_mode !== undefined) {
      resolved.ctrl_mode = CtrlMode.Resolve(msg.ctrl_mode)
    }
    else {
      resolved.ctrl_mode = new CtrlMode()
    }

    return resolved;
    }
};

class SetCtrlModeResponse {
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
    // Serializes a message object of type SetCtrlModeResponse
    // Serialize message field [success]
    bufferOffset = _serializer.bool(obj.success, buffer, bufferOffset);
    // Serialize message field [message]
    bufferOffset = _serializer.string(obj.message, buffer, bufferOffset);
    return bufferOffset;
  }

  static deserialize(buffer, bufferOffset=[0]) {
    //deserializes a message object of type SetCtrlModeResponse
    let len;
    let data = new SetCtrlModeResponse(null);
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
    return 'mindbot_msgs/SetCtrlModeResponse';
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
    const resolved = new SetCtrlModeResponse(null);
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
  Request: SetCtrlModeRequest,
  Response: SetCtrlModeResponse,
  md5sum() { return 'f628f1e6fadee58433a3f4d581cef831'; },
  datatype() { return 'mindbot_msgs/SetCtrlMode'; }
};
