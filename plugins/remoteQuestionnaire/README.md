# MindBot ESM receiver

This VSM extension is part of the [MindBot project](https://www.mindbot.eu).

During the interaction with the workers, the avatar will ask to answer a few questions from the ESM (Experience Sampling Method) questionnaire.
This plugin is used to receive the answers of the worker and store them in a local Database, which will be later integrated in the overall centralized database of ESM collected during the whole study period.

## Configuration

Instantiate an agent for the plugin `RemoteQuestionnaire` and set the `port` parameter (default 5002). This is the port where the websocket server with listen for incoming connections.

On the Unity side, check if the corresponding port is the same.

## Communication protocol

The VSM plugin sends requests to answer to a specifiy question as JSON message to the Unity client, with format:

```JSON
{
  "action_id": "<int>",
  "command": "question",
  "parameters": "text=Ti senti in controllo della situazione?"
}
```

Once the client answers the question, it sends back a JSON message like:


```JSON
{
  "action_id": "<int>",
  "question_text": "<string>",
  "answer": "<int>"
}
```

(In future, for other commands, the action_id will be the same, but other parameters will likely change)

## Save format

The answers will be logger in CSV files with the following header:

* `action_id` (int) is an incremental index indicating the call number within the runtime session;
* `timestamp` (int) indicated the moment when the user gave the answer. It is expressed as Unix-time, i.e., the number of milliseconds elapsed from 01.01.1970;
* `datetime` (string) a more user-friendly formatted version of the timestamp, in yymmdd - hhmmss;
* `question_text` (string) the text of the question;
* `answer` (int) the answer of the user, in a scale from 0 to 6.
