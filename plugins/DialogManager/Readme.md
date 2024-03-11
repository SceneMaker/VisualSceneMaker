DialogManager is a plugin that is currently focusing on connecting with Azure Speech Recognition to convert speech to text.

In the corresponding example project (at `plugins/DialogManager/ExampleProject`), there is a UDP socket created at 127.0.0.1:5000
which is listening for the ASR input (currently from the [Ubidenz app](https://gitlab.com/charamel/ubidenz)).

To use plugin:
Place ExampleProject `files in app/src/main/assets/vsm/EmmaAgent` folder in the [Ubidenz app](https://gitlab.com/charamel/ubidenz).

Current Issues:
* Stopping ASR when character is speaking.
* Dealing with long pauses that don't indicate the end of a turn.