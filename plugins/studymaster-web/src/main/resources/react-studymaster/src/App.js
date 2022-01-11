import React, {useEffect, useState} from 'react';
import './App.css';
import 'bootstrap/dist/css/bootstrap.min.css';
import {Container} from "react-bootstrap";
import InfoLogUnit from "./components/infoLogUnit";
import InputSheetUnit from "./components/inputSheetUnit";


//TODO set up user instruction in inputSheetUnit
//TODO set up scrollable information log
//TODO cleanup css with modern design

function App() {
    const [vsmConnectionStatus, setVsmConnectionStatus] = useState("");
    const [webSocket, setWebSocket] = useState(new WebSocket('ws://' + document.location.host + '/ws'));
    const [infoLogContents, setInfoLogContents] = useState();
    const [inputSheetFieldDetails, setInputSheetFieldDetails] = useState();
    const [userSubmittedInfo, setUserSubmittedInfo] = useState(new Map());
    const updateUserSubmittedInfo = (k, v) => {
        setUserSubmittedInfo(new Map(userSubmittedInfo.set(k, v)));
    }

    useEffect(() => {
        let ws = new WebSocket('ws://' + document.location.host + '/ws');

        setVsmConnectionStatus('Connecting...');
        ws.onopen = function () {
            setVsmConnectionStatus('Connected!');
        };
        ws.onclose = function () {
            setVsmConnectionStatus('Lost connection');
        };
        ws.onmessage = function (msg) {
            console.log(msg.data);
            const parts = msg.data.split('#');
            const command = parts[1];
            if (command === "REQUEST") {
                setInputSheetFieldDetails({
                    action: command,
                    variable: parts[3].split(';'),
                    options: parts[4].split(';'),
                    type: parts[5].split(';'),
                    timestamp: parts[2]
                })
            }
            if (command === "INFORM") {
                setInfoLogContents({
                    action: command,
                    variable: parts[3],
                    message: parts[4],
                    type: parts[5]
                })
            }
            setVsmConnectionStatus("Connected");
        };
        setWebSocket(ws);
        document.title = "VSM StudyMaster";
        var link = document.querySelector("link[rel*='icon']") || document.createElement('link');
        link.type = 'image/x-icon';
        link.rel = 'shortcut icon';
        link.href = 'http://scenemaker.dfki.de/images/scenemaker/logo.png';
        document.getElementsByTagName('head')[0].appendChild(link);
    }, []);


    function sendSubmitToVsm(e) {
        let areAllFieldsSet = extractVerifyAndSendUserInput(e);

        if (areAllFieldsSet) {
            webSocket.send(`VSMMessage#VAR#request_result#SUBMIT`);
            setInputSheetFieldDetails({
                action: "SUCCESSFULSEND",
                timestamp: inputSheetFieldDetails.timestamp,
            })
        } else {
            window.location.reload();
        }
    }

    function sendCancelToVsm() {
        webSocket.send(`VSMMessage#VAR#request_result#CANCEL`);

        // Resetting form to empty
        setInputSheetFieldDetails(undefined);
    }

    // If submit or cancel button is being pushed:
    // Send all variables with the selected or submitted value from user input.
    function extractVerifyAndSendUserInput(e) {
        e.preventDefault();
        let areAllFieldsSet = true;
        let i;
        for (i = 0; i < inputSheetFieldDetails.variable.length; i++) {
            let variable = inputSheetFieldDetails.variable[i];
            if (inputSheetFieldDetails.type[i] === "radio") {
                let j;
                let values = inputSheetFieldDetails.options[i].split(',');
                let radio_checked = false;
                for (j = 0; j < values.length; j++) {
                    let value = values[j];
                    let radioButtonValue = document.getElementById(value);
                    if (radioButtonValue.checked) {
                        radio_checked = true;
                    }
                }
                if (!radio_checked) {
                    //TODO Replace alert with modern warnings under the component
                    alert("Please ensure to fill in radio input for " + inputSheetFieldDetails.variable[i]);
                    areAllFieldsSet = false;
                }
            } else if (inputSheetFieldDetails.type[i] === "text") {
                if (!userSubmittedInfo.has(variable)) {
                    alert("Please ensure to fill in text input for " + inputSheetFieldDetails.variable[i]);
                    areAllFieldsSet = false;
                }
            } else if (inputSheetFieldDetails.type[i] === "number") {
                if (!userSubmittedInfo.has(variable)) {
                    alert("Please ensure to fill in number input for " + inputSheetFieldDetails.variable[i]);
                    areAllFieldsSet = false;
                }
            }
        }

        if (areAllFieldsSet) {
            for (i = 0; i < inputSheetFieldDetails.variable.length; i++) {
                let variable = inputSheetFieldDetails.variable[i];
                if (inputSheetFieldDetails.type[i] === "radio") {

                    let j;
                    let values = inputSheetFieldDetails.options[i].split(',');
                    for (j = 0; j < values.length; j++) {
                        let value = values[j];
                        let radioButtonValue = document.getElementById(value);
                        if (radioButtonValue.checked) {
                            webSocket.send(`VSMMessage#VAR#${variable}#${value}`);
                        }
                    }
                } else if (inputSheetFieldDetails.type[i] === "text") {
                    if (userSubmittedInfo.has(variable)) {
                        webSocket.send(`VSMMessage#VAR#${variable}#${userSubmittedInfo.get(variable)}`);
                    }
                } else if (inputSheetFieldDetails.type[i] === "number") {
                    if (userSubmittedInfo.has(variable)) {
                        webSocket.send(`VSMMessage#VAR#${variable}#${userSubmittedInfo.get(variable)}`);
                    }
                } else if (inputSheetFieldDetails.type[i] === "checkbox") {
                    if (document.getElementById(variable).checked) {
                        webSocket.send(`VSMMessage#VAR#${variable}#true`);
                    } else {
                        webSocket.send(`VSMMessage#VAR#${variable}#false`);
                    }
                }
            }
        }

        return areAllFieldsSet
    }

    return (
        <div className="App">
            <header className="App-header">
                <div className="wrapper-collapsed">
                    <div className="header box">
                        <h1>Studymaster</h1>
                    </div>

                    <div className="sidebar box">
                        <InfoLogUnit infoLogContents={infoLogContents}/>
                    </div>

                    <div className="content box">
                        <InputSheetUnit inputSheetFieldDetails={inputSheetFieldDetails}
                                        updateUserSubmittedInfo={updateUserSubmittedInfo}
                                        sendSubmit={sendSubmitToVsm} sendCancel={sendCancelToVsm}/>
                    </div>

                    <div className="footer box">
                        <h3>Connection status = {vsmConnectionStatus}</h3>
                    </div>

                </div>
            </header>
        </div>
    );
}

export default App;
