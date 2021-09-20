# MindBot ESM receiver

This VSM extension is part of the [MindBot project](https://www.mindbot.eu).

During the interaction with the workers, the avatar will ask to answer a few questions from the ESM (Experience Sampling Method) questionnaire.
This plugin is used to receive the answers of the worker and store them in a local Database, which will be later integrated in the overall centralized database of ESM collected during the whole study period.

## Configuration

TODO

## Communication protocol

The client sends information to the websocket server as JSON packets.
Template format is

```JSON
{
  "call_id": 2,
  "user_id": 4,
  "timestamp":  1632150588496,
  "question_id": 13,
  "vote": 5
}
```

where:

* `call_id` (int) is an incremental index indicating the call number within the runtime session;
* `user_id` (string) is a unique identifier for the user responding to the question;
* `timestamp` (int) indicated the moment when the user gave the answer. It is expressed as Unix-time, i.e., the number of milliseconds elapsed from 01.01.1970;
* `question_id` (int) a unique identifier of the question (For a list of the possible questions, see D5.1, section T5.9)
* `vote` (int) the answer of the user, in a scale from 0 to 6 (See D5.1, section T5.9 for details)

## Save format

TODO
