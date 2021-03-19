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
```VSMMessage#REQUEST#<timestamp>#<var>#<values>#<type>```  
Request values the user can choose from or can write his answer.
Var, values and type are semicolon separated, so that multiple variables can be placed in one request. 
Every variable has to have one or multiple values, which are then comma separated inside one variable, and also one type.

Type can either be text, radio or checkbox:
* text: This will be one input field which will have as an placeholder the text you type in values.
* radio: This will be one or multiple radio button, where only one can be selected for one variable. Write the label for the button in values.
* checkbox: This will be one checkbox, which can be checked or unchecked. Write the label for the checkbox in values.


Example:
```VSMMessage#REQUEST#1615553956654#name;surname;weight;gender;graduated#EnterName;EnterSurname;in kg;m,f,d;yes#text;text;text;radio;checkbox```

This example would look like this in the VisualSceneMaker:
```agent: [REQUEST var='name;surname;weight;gender;graduated' values='EnterName;EnterSurname;in kg;m,f,d;yes' type='text;text;text;radio;checkbox'].```