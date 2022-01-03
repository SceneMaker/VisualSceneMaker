import React, {useEffect, useState} from 'react';
import './App.css';
import 'bootstrap/dist/css/bootstrap.min.css';
import Button from 'react-bootstrap/Button';
import {Col, Container, Row} from "react-bootstrap";

function App() {
    const [connectionStatusText, setConnectionStatusText] = useState("");
    const [webSocket, setWebSocket] = useState(new WebSocket('ws://' + document.location.host + '/ws'));
    const [informContents, setInformContents] = useState();
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
            if (command === "INFORM") {
                setInformContents({
                    action: command,
                    variable: parts[3],
                    contents: parts[4],
                    type: parts[5]
                })
            }
            setConnectionStatusText(msg.data);
        };
        setWebSocket(ws);
        document.title = "VSM StudyMaster";
        var link = document.querySelector("link[rel*='icon']") || document.createElement('link');
        link.type = 'image/x-icon';
        link.rel = 'shortcut icon';
        link.href = 'http://scenemaker.dfki.de/images/scenemaker/logo.png';
        document.getElementsByTagName('head')[0].appendChild(link);
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
                <Row style={{
                    marginTop: "20px",
                    marginBottom: "20px"
                }}>
                    <Col xs={2}>
                        <label> {formContents.variable[i]} </label>
                    </Col>
                    <Col>
                        <input type="text" name={variable} placeholder={formContents.options[i]} id={variable}
                               onChange={e => inputValue.set(variable, e.target.value)}/>
                    </Col>
                </Row>
            )
        } else if (formContents.type[i] === "number") {
            return (
                <Row style={{
                    marginTop: "20px",
                    marginBottom: "20px"
                }}>
                    <Col xs={2}>
                        <label> {formContents.variable[i]} </label>
                    </Col>
                    <Col>
                        <input type="number" min={1} max={300} style={{"width": "150px"}} name={variable} placeholder={formContents.options[i]} id={variable}
                               onChange={e => inputValue.set(variable, e.target.value)}/>
                    </Col>
                </Row>
            )
        } else if (formContents.type[i] === "radio") {
            let values = formContents.options[i].split(',');
            return (
                <Row style={{
                    marginTop: "20px",
                    marginBottom: "20px"
                }}>
                    <Col xs={2}>
                        <label> {formContents.variable[i]} </label>
                    </Col>
                    <Col>
                        <Row>
                            {values.map((option) =>
                                <Col xs={1}>
                                    <input type="radio" id={option} name={variable} value={option}/>
                                    <label style={{"marginLeft": "10px"}}> {option} </label>
                                </Col>
                            )}
                        </Row>
                    </Col>
                </Row>
            )
        } else if (formContents.type[i] === "checkbox") {
            return (
                <Row style={{
                    marginTop: "20px",
                    marginBottom: "20px"
                }}>
                    <Col xs={2}>
                        <label> {formContents.variable[i]} </label>
                    </Col>
                    <Col>
                        <input type="checkbox" id={variable} name={variable} value={formContents.options[i]}/>
                        <label style={{"marginLeft": "10px"}}> {formContents.options[i]} </label>
                    </Col>
                </Row>
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
            <div>
                {returnValue}
            </div>
        )
    }


    return (
        <div className="App">
            <header className="App-header">
                <Container>
                    <Row style={{
                        minHeight: "400 px",
                    }}>
                        <div className="" style={{
                            minHeight: "400 px",
                            backgroundColor: "black"
                        }}>
                            <div className="h-100 d-inline-block">
                                {(informContents && (informContents.action === "INFORM")) &&
                                <h1 style={{color: "white"}}>{informContents.contents}</h1>}
                            </div>
                        </div>
                    </Row>
                    <Row>
                        <form style={{
                            height: "200 px",
                            backgroundColor: "silver"
                        }}>
                            <fieldset>
                                {(formContents && (formContents.action === "REQUEST")) && generateFields()}
                                {
                                    (formContents && (formContents.action === "SUCCESSFULSEND")) &&
                                    <div>
                                        <h2 style={{color: "green"}}>Successfully posted!</h2>
                                    </div>
                                }
                            </fieldset>
                            {
                                (formContents && (formContents.action === "REQUEST")) &&
                                <Row style={{
                                    margin: "10px"
                                }}>
                                    <Row xs={2}>
                                        {/*<Col style={} variant="primary">*/}
                                            <Button as={Col} variant="primary" onClick={sendSubmit}> Submit</Button>
                                        {/*</Col>*/}
                                        {/*<Col style={}>*/}
                                            <Button as={Col} variant="secondary" onClick={sendCancel}> Cancel</Button>
                                        {/*</Col>*/}
                                    </Row>
                                </Row>
                            }
                            {
                                !(formContents && (formContents.action === "REQUEST")) &&
                                <div>
                                    <p style={{color: "white"}}>No active requests.</p>
                                </div>
                            }
                        </form>
                    </Row>
                </Container>
            </header>
        </div>
    );
}

export default App;
