using System.Collections;
using System.Collections.Generic;
using UnityEngine;

using System.Threading.Tasks; // Task
using System.Threading;  // CancellationToken
using System.Text; // Encoding
using System; // ArraySegment


using NativeWebSocket;

using System.Text.RegularExpressions;


/** A WebSocket-based client communicating with VisualSceneMaker.
 * Attach this script to a YALLAH character in order to drive it with SceneMaker.
 * 
 * Supported commands:
 * TODO
 * 
 */
public class VSMRemoteConnector : MonoBehaviour
{
    public String ServerAddress = "127.0.0.1";
    public int ServerPort = 4000;
    public float ReconnectDelaySecs = 2.0f;


    /** The websocket used for communication. Send messages with:
     * <pre>
     * Task s = _webSock.SendText(msg);
     * s.Wait();
     * </pre>
     */
    private WebSocket _webSock;


    private MaryTTSController _ttsController;

    private AnimationController _animController;

    private FacialExpressionsController _facialExprController;

    private EyeHeadGazeController _eyeGazeController;


    class MaryTTSCallback : MaryTTSController.MaryTTSListener
    {
        private WebSocket _webSock;

        public MaryTTSCallback(WebSocket webSock)
        {
            this._webSock = webSock;
        }

        public void SpeechFinished(System.Object param)
        {
            if(this._webSock.State == WebSocketState.Open)
            {
                // we know that the parameter is an int.
                // just convert it into a string
                string msg = "@" + param.ToString();
                Debug.Log("Speech FInished. Sending '" + msg + "'");
                this._webSock.SendText(msg);
            }
        }
    }

    private MaryTTSCallback _ttsListener;

    // Start is called before the first frame update
    void Start()
    {
        //
        // Lookup for the components to speak and animate the character

        this._ttsController = gameObject.GetComponentInChildren<MaryTTSController>();
        Debug.Assert(this._ttsController != null, "Couldn't find the TTSController (text to speech) in the avatar.");

        this._animController = gameObject.GetComponent<AnimationController>();
        Debug.Assert(this._animController != null, "Couldn't find the AnimationController in the avatar.");

        this._facialExprController = gameObject.GetComponentInChildren<FacialExpressionsController>();
        Debug.Assert(this._facialExprController != null, "Couldn't find the FacialAnimationController in the avatar.");

        this._eyeGazeController = gameObject.GetComponentInChildren<EyeHeadGazeController>();
        Debug.Assert(this._eyeGazeController != null, "Couldn't find the EyeHeadGazeController in the avatar.");

        //
        // Now initialize the WebSocket

        // It is important to have the trailing slash!
        String serverURL = "ws://" + this.ServerAddress + ":" + this.ServerPort + "/";
        Debug.Log("WebSocket Server URL is '" + serverURL + "'.");

        _webSock = new WebSocket(serverURL);

        _webSock.OnMessage += (bytes) =>
        {
            // Reading a plain text message
            var message = System.Text.Encoding.UTF8.GetString(bytes);
            // Debug.Log("WebSocket Got message: " + message);
            _HandleMessage(message);
        };

        _webSock.OnError += (msg) =>
        {
            // Reading a plain text message
            //var message = System.Text.Encoding.UTF8.GetString(bytes);
            Debug.Log("WebSocket Error: " + msg);
        };

        _webSock.OnClose += (msg) =>
        {
            Debug.Log("WebSocket Connection closed: " + msg);
        };


        // BEWARE!!! In UnityEditor `Connect` is blocking!
        // It will unlock only when the socket closes.
        //await _webSock.Connect();

        // Can only use the await form. Explicit use of Task hangs the system
        /*Task t = _webSock.Connect();
        t.Wait();
        Debug.Log(t.Status);
        if (t.IsFaulted)
        {
            Debug.Log(t.Exception);
        }
        Debug.Log("Connected.");
        */

        Debug.Log("WebSocket State: " + _webSock.State);


        // Instantiate the listener
        this._ttsListener = new MaryTTSCallback(this._webSock);
        this._ttsController.AddListener(this._ttsListener);

    }

    private async void OnApplicationQuit()
    {
        await _webSock.Close();
    }


    private float _socketLastCheckTime = 0.0f;

    // Update is called once per frame
    void Update()
    {
#if !UNITY_WEBGL || UNITY_EDITOR
        // The following test is mandatory, to prevent Unity (Editor)
        // giving NullPointer messages after a certain inactivity while disconnected.
        //if (_webSock.State == WebSocketState.Open)
        {
            _webSock.DispatchMessageQueue();
        }
#endif
        float now = Time.time;
        float socket_unchecked_for = now - this._socketLastCheckTime;


        if (socket_unchecked_for > ReconnectDelaySecs)
        {
            this._socketLastCheckTime = now;
            if (_webSock.State == WebSocketState.Closed)
            {
                Debug.Log("Socket closed for more than " + ReconnectDelaySecs + " seconds. Reconnecting...");
                Invoke("_OpenSocket", 0);
                this._socketLastCheckTime = Time.time;
            }
        }

    }



    private async void _OpenSocket()
    {
        if (_webSock.State == WebSocketState.Closed)
        {
            await _webSock.Connect();
            Debug.Log("Connect Terminated.");
        }
    }

    private async void _CloseSocket()
    {
        if (_webSock.State == WebSocketState.Open)
        {
            await _webSock.Close();
            Debug.Log("Socket Closed.");
        }

    }


#region JSON support classes
    //
    // These are the support classes to parse the JSON messages coming from VSM.

    [System.Serializable]
    class VSMmessage
    {
        /** The mesage type: "text", "command", ... */
        public string type;
    }

    [System.Serializable]
    class VSMTextMessage: VSMmessage
    {
        public string text;
        public string id;
    }

    [System.Serializable]
    class VSMCommandMessage: VSMmessage
    {
        public string command;
        public string parameters;
    }

#endregion


    private void _HandleMessage(string msg)
    {
        //Debug.Log("Received JSON: " + msg);

        VSMmessage vsm_msg = JsonUtility.FromJson<VSMmessage>(msg);
        //Debug.Log("Message type: " + vsm_msg.type);

        if (vsm_msg.type == "text")
        {
            VSMTextMessage text_msg = JsonUtility.FromJson<VSMTextMessage>(msg);
            Debug.Log("Built JSON: " + JsonUtility.ToJson(text_msg));

            this._handleText(text_msg.text, int.Parse(text_msg.id));

/*
            // Identify the VSM markers
            string[] msg_words = text_msg.text.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);

            // Collect the list of markers and pure text.
            string text_only = "";
            LinkedList<string> markers = new LinkedList<string>();
            foreach (string s in msg_words)
            {
                if (s.StartsWith("$"))
                {
                    markers.AddLast(s);
                }
                else
                {
                    text_only += " " + s;
                }
            }
            // Trim leading space.
            text_only = text_only.Trim();
            // Debug.Log("Saying '" + text_only + "'");

            //
            // Text can be sent to TTS synth
            if (text_only != "")
            {
                this._ttsController.MaryTTSspeak(text_only);
            }


            //
            // For each marker, send a message back to VSM
            // TODO -- this should be synchronized with the speech.
            foreach (string a in markers)
            {
                if (_webSock.State == WebSocketState.Open)
                {
                    _webSock.SendText(a);
                }
                else
                {
                    Debug.Log("WebSocket is Closed. Cannot send.");
                }
            }
*/
        }
        else if(vsm_msg.type == "command")
        {
            VSMCommandMessage cmd_msg = JsonUtility.FromJson<VSMCommandMessage>(msg);
            Debug.Log("Built JSON: " + JsonUtility.ToJson(cmd_msg));
            //Debug.Log("CommandParameters : " + cmd_msg.parameters);
            // (" + cmd_msg.parameters.Count + ")

            // Unpack the parameters
            string[] parameters_list = cmd_msg.parameters.Split(new char[] { ',' }, StringSplitOptions.RemoveEmptyEntries);
            Dictionary<string, string> parameters_dict = new Dictionary<string, string>();
            foreach(string param_entry in parameters_list)
            {
                string[] param_value_pair = param_entry.Split(new char[] { '=' }, StringSplitOptions.RemoveEmptyEntries);
                if (param_value_pair.Length != 2)
                {
                    Debug.LogError("Command parameter malformed. Expected 'key=value', got '" + param_entry + "'");
                }
                parameters_dict.Add(param_value_pair[0], param_value_pair[1]);
            }
            // Debug.Log("Got " + parameters_dict.Count + " parameters");

            //
            // And invoke the command
            this._handleCommand(cmd_msg.command, parameters_dict);

        }
        else
        {
            Debug.LogError("Received VSM message of unknown type '" + vsm_msg.type + "'. Ignoring...");
        }

    }


    private readonly string MARKER_REGEXP = "\\$\\d+" ;

    private void _handleText(string txt, int id)
    {


        //
        // Identify the markers
        MatchCollection matches = Regex.Matches(txt, MARKER_REGEXP);
        LinkedList<string> markers = new LinkedList<string>();
        foreach (Match m in matches)
        {
            markers.AddLast(m.Value);
        }

        // Remove the markers from the text
        string text_only = Regex.Replace(input: txt, pattern: MARKER_REGEXP, replacement: "");

        //
        // Text can be sent to TTS synth
        if (text_only != "" && text_only != ".")
        {
            Debug.Log("Found " + markers.Count + " markers.\tSaying text: '" + text_only + "'");
            this._ttsController.MaryTTSspeak(text_only, id);
        }


        //
        // For each marker, send a message back to VSM
        // TODO -- this should be synchronized with the speech.
        foreach (string a in markers)
        {
            if (_webSock.State == WebSocketState.Open)
            {
                _webSock.SendText(a);
            }
            else
            {
                Debug.Log("WebSocket is Closed. Cannot send.");
            }
        }

    }


    private void _handleCommand(string cmd, Dictionary<string, string> parameters)
    {
        // PlayAnimationClip
        // * name: string
        if (cmd == "PlayAnimationClip") {
            string name = parameters["name"];
            // Debug.Log("Playclip " + name);
            this._animController.PlayAnimationClip(name);
        }
        // SetCurrentFacialExpression
        // * name: string
        else if (cmd == "SetCurrentFacialExpression")
        {
            string name = parameters["name"];
            this._facialExprController.SetCurrentFacialExpression(name);
        }
        // ClearFacialExpression
        else if (cmd == "ClearFacialExpression")
        {
            this._facialExprController.ClearFacialExpression();

        }
        // LookAtObject
        // * name: string
        else if (cmd == "LookAtObject")
        {
            string name = parameters["name"];
            this._eyeGazeController.LookAtObject(name);

        }
        // LookAtPoint
        // * x: float
        // * y: float
        // * z: float
        else if (cmd == "LookAtPoint")
        {
            float x = float.Parse(parameters["x"]);
            float y = float.Parse(parameters["y"]);
            float z = float.Parse(parameters["z"]);
            this._eyeGazeController.LookAtPoint(x, y, z);
        }
        // StopLooking
        else if (cmd == "StopLooking")
        {
            this._eyeGazeController.StopLooking();

        }
        else
        {
            Debug.LogError("Unknown command '" + cmd + "'");
        }
    }

}