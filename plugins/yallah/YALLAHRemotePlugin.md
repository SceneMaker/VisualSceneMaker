# VSM plugin for communication with YALLAH

This VSM plugin implements the communication with a YALLAH character in Unity.

The communication uses WebSockets, allowing for a communication also with WebPages.

## Message exchange dynamics

Between `YALLAH <-- and --> VSM`
* `<--` VSM parse a sentence:
  * substitutes actions with markers;
  * split it according to punctuation;
  * and send one message per clause;
  * when a marker mesage is received, converts it into action and send a message with action name and parameters.
* `-->` YALLAH must:
  * use the text to speak out the sentence;
  * send a message to VSM each time a marker is met;
  * send a message to VSM when the sentence has been spoken out completely.

Example: "Bye [PlayAnimationClip name=wave], and hope to see [PlayAnimationClip name=point_forward] you soon."

* `<--` text: "Bye $1", id: 23
* `-->` "$1"
* `<--` command: "PlayAnimationClip", parameters: "name=wave"
* `-->` "@23"
* `<--` "and hope to see $2 you soon", id: 25
* `-->` "$2"
* `<--` command: "PlayAnimationClip", parameters: "name=point_forward"
* `-->` "@24"
* at this point VSM can switch to the next state

_Beware: text speech is blocking, actions NOT! If an action is still executing while the sentence finishes, it might overlap with actions of the next sentence._

    
## Messages

Messages traveling from VSM to YALLAH can be of 2 types: text or command.

* A text is the text with the actions substituted by markers.
  * Example:
  "Bye [PlayAnimationClip name=wave]. Stay well, and hope to see [PlayAnimationClip name=point_forward] you soon.",
  will be sent to the YALLAH agent as:
  "Bye $1. Stay well, and hope to see $2 you soon."
* A command is the translation of the markers into actions
  * Example: [PlayAnimationClip name=wave]

All messages will be structured as JSON strings

## Message structure

The messages from YALLAH to VSM a re simple text strings:

* `$xx` (where xx is an integer number) is to notify that an action marker was reached;
* `@xx` (where xx is an integer number) is to notify that a sentence utterance has finished. 

The following are the structure of the JSON messages from VSM to YALLAH.

For text messages:

```JSON
{
  "type": "text",
  "id": "integer_unique_id",
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
  * _no parameters_

* LookAtObject
  * name: string

* LookAtPoint
  * x: float
  * y: float
  * z: float
  
* StopLooking
  * _no parameters_
