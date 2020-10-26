# VSM plugin for communication with YALLAH

This VSM plugin implements the communication with a YALLAH character in Unity.

The communication is based on WebSockets, allowing for a communication also with WebPages.


## Messages

Messages can be of 2 types: text or command.

* A text is the text with the actions substituted by markers.
  * Example:
  "Bye [PlayAnimationClip name=wave]. Stay well, and hope to see [PlayAnimationClip name=point_forward] you soon.",
  will be sent to the YALLAH agent as:
  "Bye $1. Stay well, and hope to see $2 you soon."
* A command is the translation of the markers into actions
  * Example: [PlayAnimationClip name=wave]

All messages will be structured as JSON strings

## Message structure

For text messages:

```JSON
{
  "type": "text",
  "text": "Bye $1. Stay well, and hope to see $2 you soon."
}
```


For commands:

```JSON
{
  "type": "command",
  "command": "PlayAnimationClip",
  "parameters": "name=wave"
}
```

Parameters formatting reason. It would be more elegant to encode the parameters as a dictionary.
However, Unity doesn't integrate a full-fledged json parser. The provided `UnityEngine.JsonUtility` supports only serialization to-from classes, with no support for variable structures.
Hence, we leave the parameters in the key1=value,key2=value,... format and leave the unpacking to the Unity side.

So, for example, when more parameters are needed:

```JSON
{
  "type": "command",
  "command": "LookAtPoint",
  "parameters": "x=1.5,y=2.1,z=1.0"
}
```


## Supported commands

This is a subset of the API provided by the YALLAH controllers <https://github.com/yallah-team/YALLAH/wiki/Developer-UnityAPI>.

* PlayAnimationClip
  * name: string

* SetCurrentFacialExpression
  * name: string

* ClearFacialExpression

* LookAtObject
  * name: string

* LookAtPoint
  * x: float
  * y: float
  * z: float
  
* StopLooking
