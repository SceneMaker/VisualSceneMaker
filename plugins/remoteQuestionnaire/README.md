# MindBot ESM receiver

This VSM extension is part of the [MindBot project](https://www.mindbot.eu).

During the interaction with the workers, the avatar will ask to answer a few questions from the ESM (Experience Sampling Method) questionnaire.
This plugin is used to receive the answers of the worker and store them in a local Database, which will be later integrated in the overall centralized database of ESM collected during the whole study period.

In general, this plugin offers a mechanism to ask Likert-like questions and receive answers.

![Example VSM project](MindBot-VSM-TestESM.png)

A client is implemented as Unity package.

![Example implementation](MindBot-AvatarPlayer-ESM.png)


## Configuration

Instantiate an agent for the plugin `RemoteQuestionnaire` and set the `port` parameter (default 5002). This is the port where the websocket server with listen for incoming connections.

![Example configuration](MindBot-VSM-ConfigureQuestionnairePlugin.png)

Questions can be triggered through a scene text:

```
scene @@ ESM1
quest: [Question text='Sei ben concentrato?'].

scene @@ ESM2
quest: [Question text='Ti senti in controllo della situazione?'].
```


## Communication protocol

The VSM plugin sends requests to answer to a specific question as JSON message to the client, with the format:

```JSON
{
  "action_id": "<int>",
  "command": "question",
  "parameters": "text=<string>"
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

Where the `question_text` is supposed to be the same text displayed to the user,
and the `answer` an integer from, e.g., 0 to 6 (for 7-level Liker scales) indicating the chosen answer.

(In the future, for other commands, the action_id will stay, but other parameters will likely change)

## Save format

The answers will be logged in CSV files (_TAB separated_) with the following header:

* `timestamp` (int) indicates the moment when the user gave the answer. It is expressed as Unix-time, i.e., the number of milliseconds elapsed from 01.01.1970;
* `datetime` (string) a more user-friendly formatted version of the timestamp, in yymmdd - hhmmss;
* `question_text` (string) the text of the question;
* `answer` (int) the answer of the user, in a scale from 0 to 6.

Example:
```
timestamp	datetime	question_text	answer
1667860136732	20221107-Mon-232856	Sei ben concentrato?	2
1667860147419	20221107-Mon-232907	Ti senti in controllo della situazione?	4
```

## TODO

* A command to set the number of answers and their labels
