# VSM plugin for communicating with YALLAH

This VSM plugin implements the communication with a YALLAH character in Unity.

The communication uses WebSockets, allowing for a communication also when the YALLAH project is deployed as WebGL page.

For the YALLAH counterpart, see:

* The main YALLAH project at <https://github.com/yallah-team/YALLAH>
* The C# script(s) at <https://github.com/yallah-team/YALLAH/tree/master/UnityProjects/YallahTestbed/Assets/YALLAH/Scripts/vsm>

## Configuration

After opening/creating a project, the plugin can be used by:

* Add a Device of type `YallahExecutor` with parameters.
  * `port` This is the tcp port that the WebSocket server will use for listening to incoming connection. The same port number must be configured in the YALLAH side as `ServerPort` component attribute.
  * `launchMode` This can be configured to force starting a YALLAH instance. Three values are possible:
    * `None` We assume the YALLAH Unity application will be started manually;
    * `App` (Still unsupported) The path to an executable file that will be launched;
    * `WebPage` (Still unsupported) The application will be opened as a WebGL application and served from the specified HTML page.
* Add an Agent on that device
  * Only 1 agent is supported. If you want to drive more agents, then add another device listening to a different port.

## Message exchange dynamics

Between `YALLAH <-- and --> VSM`
* When a scene sentence is executed, VSM must:
  * parse the sentence;
  * substitutes actions with markers;
  * split it according to punctuation;
  * `<--` send one message per clause.
* On message reception, YALLAH must:
  * use the text to speak out the sentence;
  * `-->` send a message to VSM each time a marker is met;
  * `-->` send a message to VSM when the sentence has been spoken out completely.
* When a marker message is received (e.g., $1), VSM must:
  * `<--` retrieve the action from the marker number and send a message with action name and parameters.

Example: "Bye [PlayAnimationClip name=wave], and hope to see [PlayAnimationClip name=point_forward] you soon."

* `<--` text: "Bye $1", id: 23
* `-->` "$1"
* `<--` command: "PlayAnimationClip", parameters: "name=wave"
* `-->` "@23"
* `<--` "and hope to see $2 you soon", id: 24
* `-->` "$2"
* `<--` command: "PlayAnimationClip", parameters: "name=point_forward"
* `-->` "@24"
* at this point VSM can switch to the next state

_Beware: text speech is blocking, actions NOT! If an action is still executing while the sentence finishes, it might overlap with actions of the next sentence._

    
## Messages

Messages traveling to YALLAH `<--` from VSM can be of 2 types: text or command.

* A **text** is the text with the actions substituted by markers.
  * Example:
  "Bye [PlayAnimationClip name=wave]. Stay well, and hope to see [PlayAnimationClip name=point_forward] you soon.",
  will be sent to the YALLAH agent as:
  "Bye $1. Stay well, and hope to see $2 you soon."
* A **command** is the translation of the markers into actions
  * Example: [PlayAnimationClip name=wave]

All messages will be structured as JSON strings

## Message structure

The messages from YALLAH `-->` to VSM are simple text strings:

* `$xx` (where xx is an integer number) is to notify that an action marker was reached;
* `@xx` (where xx is an integer number) is to notify that a sentence utterance has finished. 

The following are the structure of the JSON messages from VSM to YALLAH.

For text messages:

```JSON
{
  "type": "text",
  "id": "23",
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
However, Unity doesn't integrate a full-fledged JSON parser. The provided `UnityEngine.JsonUtility` supports only serialization to/from classes, with no support for variable structures.
Hence, we leave the parameters in the key1=value,key2=value,... format and leave the unpacking to the Unity side.

So, for example, when more parameters are needed, we send:

```JSON
{
  "type": "command",
  "command": "LookAtPoint",
  "parameters": "x=1.5,y=2.1,z=1.0"
}
```


## Supported commands

This is a subset of the API provided by the YALLAH controllers (see <https://github.com/yallah-team/YALLAH/wiki/Developer-UnityAPI>).

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


## Known problems and limitations

* The speech and the execution of the actions (sending the markers back to VSM) is not implemented, yet. All markers are sent back immediately;
* It is still missing the synchronisation when there are more clauses in a line. E.g., in sentence "Hello, I am Anna", the "Hello" is not heard because it is immediately overridden by the "I am Anna";
* Cannot playback animation containing a space in their name, neither look at a camera with a space in its name. See VSM issue #200: <https://github.com/SceneMaker/VisualSceneMaker/issues/200>;
* Known bug: in WebGL, the YALLAH socket doesn't reconnect if the VSM server stops the project and restarts. Workaround: stop the VSM project, reload the page with the avatar, re-start the VSM project.
