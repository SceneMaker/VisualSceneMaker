# MindBot - SSI reception module

This is the plugin used to receive SSI messages, specific to the MindBot project.

## Installation:

* Open the project properties and add a device of type `de.dfki.vsm.xtension.mindbotssi.MindBotSSIPlugin`.
* Set the property variables:
  * `hlhost` - (handler local host) the local IP of the interface receiving the _handle_ messages
  * `hlport` - (handler local port) ... and port number (SSI pipelines must send messages to this host:port)
  * `slhost` - (sender local host) the local IP of the interface sending messages to the SSI pipeline
  * `slport` - (sender local port) ... and port number (SSI pipelines will receive messages from this host:port)
  * `srhost` - (sender remote host) the remote IP receiving the messages
  * `srport` - (sender remote port) ... and the port number (SSI pipelines will receive messages to this host:port

## Emotion + pain + arousal/valence + focus pipeline

Values are put in the following global variables (grouped by type).
VSM project variables are described as: `type var_name = default_value`

Emotion variables are classes of the same softmax distribution:

```
Float ssi-emotion-surprise = 0.0
Float ssi-emotion-pain = 0.0
Float ssi-emotion-happy = 0.0
Float ssi-emotion-sad = 0.0
Float ssi-emotion-neutral = 0.0
Float ssi-emotion-disgust = 0.0
Float ssi-emotion-anger = 0.0
Float ssi-emotion-fear = 0.0
```

For arousal and valence, the following two independent variables are set in range [0.0,1.0]

```
Float ssi-emotion-valence = 0.0
Float ssi-emotion-arousal = 0.0
```

The focus module is a softmax distribution among:

```
Float ssi-focus-away = 0.0
Float ssi-focus-screen = 0.0
Float ssi-focus-device = 0.0
```


## Stress detection pipeline

This is the pipeline getting an estimation of stress through the PolarBand.

The stress pipeline gives a single value in range [0.0,1.0]:

```
Float ssi-stress = 0.0
```
