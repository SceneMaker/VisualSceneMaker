import React, {useEffect, useState} from 'react';
import './App.css';


function App() {
    const [connectionStatusText, setConnectionStatusText] = useState("");
    const [webSocket, setWebSocket] = useState();
    const [formContents, setFormContents] = useState();
    const inputValue = new Map();
    useEffect(() => {
        let ws = new WebSocket('ws://' + document.location.host + '/ws');

        setConnectionStatusText('Connecting...');
        ws.onopen = function () {
            setConnectionStatusText('Connected!');
        };
        ws.onclose = function () {
            setConnectionStatusText('Lost connection');
        };
        ws.onmessage = function (msg) {
            console.log(msg.data);
            const parts = msg.data.split('#');
            const command = parts[1];
            if (command === "REQUEST") {
                setFormContents({
                    action: command,
                    variable: parts[3].split(';'),
                    options: parts[4].split(';'),
                    type: parts[5].split(';'),
                    timestamp: parts[2]
                })
            }
            setConnectionStatusText(msg.data);
        };
        setWebSocket(ws);
    }, []);


    function sendSubmit(e) {
        let allFieldSet = extractAndSendUserInput(e);

        if (allFieldSet) {
            webSocket.send(`VSMMessage#VAR#request_result#SUBMIT`);
            setFormContents({
                action: "SUCCESSFULSEND",
                timestamp: formContents.timestamp,
            })
        } else {
            window.location.reload();
        }
    }

    function sendCancel() {
        webSocket.send(`VSMMessage#VAR#request_result#CANCEL`);

        // Resetting form to empty
        setFormContents(undefined);
    }

    // If submit or cancel button is being pushed:
    // Send all variables with the selected/ written value from user input.
    function extractAndSendUserInput(e) {
        e.preventDefault();
        let allFieldsSet = true;
        let i;
        for (i = 0; i < formContents.variable.length; i++) {
            let variable = formContents.variable[i];
            if (formContents.type[i] === "radio") {
                let j;
                let values = formContents.options[i].split(',');
                let radio_checked = false;
                for (j = 0; j < values.length; j++) {
                    let value = values[j];
                    let radioButtonValue = document.getElementById(value);
                    if (radioButtonValue.checked) {
                        radio_checked = true;
                    }
                }
                if (!radio_checked) {
                    alert("Please ensure to fill in radio input for " + formContents.variable[i]);
                    allFieldsSet = false;
                }
            } else if (formContents.type[i] === "text") {
                if (!inputValue.has(variable)) {
                    alert("Please ensure to fill in text input for " + formContents.variable[i]);
                    allFieldsSet = false;
                }
            } else if (formContents.type[i] === "number") {
                if (!inputValue.has(variable)) {
                    alert("Please ensure to fill in number input for " + formContents.variable[i]);
                    allFieldsSet = false;
                }
            }
        }

        if (allFieldsSet) {
            for (i = 0; i < formContents.variable.length; i++) {
                let variable = formContents.variable[i];
                if (formContents.type[i] === "radio") {

                    let j;
                    let values = formContents.options[i].split(',');
                    for (j = 0; j < values.length; j++) {
                        let value = values[j];
                        let radioButtonValue = document.getElementById(value);
                        if (radioButtonValue.checked) {
                            webSocket.send(`VSMMessage#VAR#${variable}#${value}`);
                        }
                    }
                } else if (formContents.type[i] === "text") {
                    if (inputValue.has(variable)) {
                        webSocket.send(`VSMMessage#VAR#${variable}#${inputValue.get(variable)}`);
                    }
                } else if (formContents.type[i] === "number") {
                    if (inputValue.has(variable)) {
                        webSocket.send(`VSMMessage#VAR#${variable}#${inputValue.get(variable)}`);
                    }
                } else if (formContents.type[i] === "checkbox") {
                    if (document.getElementById(variable).checked) {
                        webSocket.send(`VSMMessage#VAR#${variable}#true`);
                    } else {
                        webSocket.send(`VSMMessage#VAR#${variable}#false`);
                    }
                }
            }
        }

        return allFieldsSet
    }


    function generateInputFieldWithType(i) {
        let variable = formContents.variable[i];
        if (formContents.type[i] === "text") {
            return (
                <>
                    <p>
                        <label> {formContents.variable[i]} </label>
                        <input type="text" name={variable} placeholder={formContents.options[i]} id={variable}
                               onChange={e => inputValue.set(variable, e.target.value)}/>
                    </p>
                </>
            )
        } else if (formContents.type[i] === "number") {
            return (
                <>
                    <p>
                        <label> {formContents.variable[i]} </label>
                        <input type="number" name={variable} placeholder={formContents.options[i]} id={variable}
                               onChange={e => inputValue.set(variable, e.target.value)}/>
                    </p>
                </>
            )
        } else if (formContents.type[i] === "radio") {
            let values = formContents.options[i].split(',');
            return (
                <>
                    <p>
                        <label> {formContents.variable[i]} </label>
                        {values.map((option) =>
                            <>
                                <input type="radio" id={option} name={variable} value={option}/>
                                <label> {option} </label>
                            </>
                        )}
                    </p>
                </>
            )
        } else if (formContents.type[i] === "checkbox") {
            return (
                <>
                    <p>
                        <label> {formContents.variable[i]} </label>
                        <input type="checkbox" id={variable} name={variable} value={formContents.options[i]}/>
                        <label> {formContents.options[i]} </label>
                    </p>
                </>
            )
        }
    }


    function generateFields() {
        let returnValue = [];

        let i;
        for (i = 0; i < formContents.variable.length; i++) {
            let inputBoxes = generateInputFieldWithType(i);
            returnValue.push(inputBoxes);
        }
        return (
            <>
                {returnValue}
            </>
        )
    }


    return (
        <div className="App">
            <header className="App-header">
                <form>
                    <fieldset>
                        {(formContents && (formContents.action === "REQUEST")) && generateFields()}
                        {
                            (formContents && (formContents.action === "SUCCESSFULSEND")) &&
                            <div>
                                <h2>Successfully posted!</h2>
                            </div>
                        }
                    </fieldset>
                    {
                        (formContents && (formContents.action === "REQUEST")) &&
                        <div>
                            <button onClick={sendSubmit}> submit</button>
                            <button onClick={sendCancel}> cancel</button>
                        </div>
                    }
                    {
                        !(formContents && (formContents.action === "REQUEST")) &&
                        <div>
                            <h2>No active requests.</h2>
                        </div>
                    }
                </form>
            </header>
        </div>
    );
}

export default App;
