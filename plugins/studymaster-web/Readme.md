# Studymaster as a web interface 
This project uses an embedded javalin web server to host a studymaster gui that 
communicates with the executor via websocket

## Protocol
### Client messages:
```VSMMessage#VAR#<var>#<value>```  
set variable <var> to value <value>

```VSMMessage#Go```  
send 'Go' message. This is usually used to hold back a scene flow that has started,
for example to hide scenemaker from study participants.

### Server messages:
```VSMMessage#REQUEST#<timestamp>#<var>#<options,..>```  
Request a value for the variable from the user. 
The user can choose one of the provided options. 
The options are comma seperated