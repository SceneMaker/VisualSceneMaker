import React, {useEffect, useState} from 'react';
import './App.css';
import 'bootstrap/dist/css/bootstrap.min.css';
import InfoLogUnit from "./components/infoLogUnit";
import InputSheetUnit from "./components/inputSheetUnit";
import LinkIcon from '@mui/icons-material/Link';
import LinkOffIcon from '@mui/icons-material/LinkOff';
import {Tooltip} from "@material-ui/core";

function genTimeStamp() {
    let today = new Date();
    let timestamp = today.getFullYear() + '-' + (today.getMonth() + 1) + '-' + today.getDate() + "_" +
        today.getHours() + ":" + today.getMinutes() + ":" + today.getSeconds();
    return timestamp;
}

function App() {
    const [vsmConnectionStatus, setVsmConnectionStatus] = useState(false);
    // const [webSocket, setWebSocket] = useState(new WebSocket('ws://' + document.location.host + '/ws'));
    let webSocket;
    const [infoLogContents, setInfoLogContents] = useState({});
    const [informContent, setInformContent] = useState("");
    const [inputSheetFieldDetails, setInputSheetFieldDetails] = useState();
    const [collapseDevToolComp, setCollapseDevToolComp] = useState(true);
    const [vsmVarsForDevToolComp, setVsmVarsForDevToolComp] = useState({});
    const [userSubmittedInfo, setUserSubmittedInfo] = useState({});


    const updateUserSubmittedInfo = (k, v) => {
        let items = {...userSubmittedInfo};
        items[k] = v;
        // console.log("!");
        // console.log("==>", k, v);
        // console.log(v);
        console.log(items);
        setUserSubmittedInfo(items);
    }

    const heartbeat = (ws, delay) => {
        clearTimeout(ws.pingTimeout)

        ws.pingTimeout = setTimeout(() => {
            ws.terminate()
        }, delay)
    }


    useEffect(() => {
        webSocket = new WebSocket('ws://' + document.location.host + '/ws');

        let ws = webSocket;

        ws.onopen = function () {
            setVsmConnectionStatus(true);
            clientAliveMessage();
            console.log("Connection initiated by server.")
        };
        ws.onclose = function (msg) {
            setVsmConnectionStatus(false);
            setInfoLogContents({});
            setUserSubmittedInfo({});
            setInputSheetFieldDetails({});
            setInformContent("");
            console.log("Connection terminated by server.")

        };
        ws.onmessage = function (msg) {

            const parts = msg.data.split('#');
            const command = parts[1];
            if (command === "REQUEST") {

                let newInputSheetFieldDetails = {
                    action: command,
                    variable: parts[3].split(';'),
                    options: parts[4].split(';'),
                    type: parts[5].split(';'),
                    timestamp: parts[2]
                };
                setInputSheetFieldDetails(newInputSheetFieldDetails);
                let obj = {};
                let newObj = newInputSheetFieldDetails.variable.reduce((obj, key) => ({...obj, [key]: ""}), {})
                // setUserSubmittedInfo({"variables": newObj});
                setUserSubmittedInfo(newObj);
            }
            if (command === "INFORM") {

                let currTS = genTimeStamp();

                let newInfo = {};
                newInfo[currTS] = [parts[4]];
                setInformContent(parts[4]);
                setInfoLogContents(Object.assign(infoLogContents, newInfo));
            }

            if (command === "UPDATE") {
                let variable = parts[2];
                let val = parts[3];
                let newVarVal = {}
                newVarVal[variable] = [val];

                setVsmVarsForDevToolComp(Object.assign(vsmVarsForDevToolComp, newVarVal));
            }
            setVsmConnectionStatus(true);
        };
        // setWebSocket(ws);
        document.title = "VSM StudyMaster";
        var link = document.querySelector("link[rel*='icon']") || document.createElement('link');
        link.type = 'image/x-icon';
        link.rel = 'shortcut icon';
        link.href = 'http://scenemaker.dfki.de/images/scenemaker/logo.png';
        document.getElementsByTagName('head')[0].appendChild(link);
    }, []);

    var aliveTimer = null;

    /**
     * Message from client (this and webpage loading this) that client is alive
     */
    function clientAliveMessage() {
        // console.log("Send client alive message to server");
        if (webSocket.readyState === WebSocket.OPEN) {
            webSocket.send(`VSMMessage#STATUS#alive`);
            aliveTimer = setTimeout(clientAliveMessage, 100);
        }

    }

    function sendSubmitToVsm() {
        let areAllFieldsSet = true;

        if (areAllFieldsSet) {

            for (let i = 0; i < inputSheetFieldDetails.variable.length; i++) {
                let variable = inputSheetFieldDetails.variable[i];
                if (inputSheetFieldDetails.type[i] === "radio") {
                    if (userSubmittedInfo.hasOwnProperty(variable)) {
                        webSocket.send(`VSMMessage#VAR#${variable}#${userSubmittedInfo[variable]}`);
                    }
                } else if (inputSheetFieldDetails.type[i] === "text") {
                    if (userSubmittedInfo.hasOwnProperty(variable)) {
                        webSocket.send(`VSMMessage#VAR#${variable}#${userSubmittedInfo[variable]}`);
                    }
                } else if (inputSheetFieldDetails.type[i] === "number") {
                    if (userSubmittedInfo.hasOwnProperty(variable)) {
                        webSocket.send(`VSMMessage#VAR#${variable}#${userSubmittedInfo[variable]}`);
                    }
                } else if (inputSheetFieldDetails.type[i] === "checkbox") {
                    if (userSubmittedInfo.hasOwnProperty(variable)) {
                        console.log(userSubmittedInfo);
                        console.log(userSubmittedInfo[variable]);
                        webSocket.send(`VSMMessage#VAR#${variable}#${userSubmittedInfo[variable]}`);
                    }
                }
            }

            webSocket.send(`VSMMessage#VAR#request_result#SUBMIT`);
            setInputSheetFieldDetails({
                action: "SUCCESSFULSEND",
                timestamp: inputSheetFieldDetails.timestamp,
            })
            setUserSubmittedInfo(new Map());
        } else {
            window.location.reload();
        }
    }

    function sendCancelToVsm() {
        webSocket.send(`VSMMessage#VAR#request_result#CANCEL`);

        for (let i = 0; i < inputSheetFieldDetails.variable.length; i++) {
            let variable = inputSheetFieldDetails.variable[i];
            if (inputSheetFieldDetails.type[i] === "radio") {
                if (userSubmittedInfo.hasOwnProperty(variable)) {
                    updateUserSubmittedInfo(variable, "");
                    // console.log(variable)
                }
            } else if (inputSheetFieldDetails.type[i] === "text") {
                if (userSubmittedInfo.hasOwnProperty(variable)) {
                    // updateUserSubmittedInfo(variable, "");
                    // console.log(variable)

                }
            } else if (inputSheetFieldDetails.type[i] === "number") {
                if (userSubmittedInfo.hasOwnProperty(variable)) {
                    updateUserSubmittedInfo(variable, "");
                    // console.log(variable)

                }
            } else if (inputSheetFieldDetails.type[i] === "checkbox") {
                if (userSubmittedInfo.hasOwnProperty(variable)) {
                    let values = inputSheetFieldDetails.options[i].split(',');
                    // setCheckBoxState(values.reduce((a, v) => ({...a, [v]: false}), {}));
                    let init_
                    updateUserSubmittedInfo(variable, values.reduce((a, v) => ({...a, [v]: false}), {}));
                    // console.log(variable)

                }
            }
        }

        // setUserSubmittedInfo(new Map(userSubmittedInfo.set("surname", "")));
        // window.location.reload();
    }


    return (
        <div className="App">
            <div className={collapseDevToolComp ? "wrapper-collapsed" : "wrapper"}>
                <div className="header box">
                    <div className="flex-container">

                        <div className="item1">
                            <h1>Studymaster</h1>
                        </div>
                        <div className="item2">
                            {vsmConnectionStatus ?
                                <Tooltip disableFocusListener title="VSM connected">
                                    <LinkIcon style={{fill: "green", fontSize: 50}}/>
                                </Tooltip> :
                                <Tooltip disableFocusListener title="VSM not connected">
                                    <LinkOffIcon style={{fill: "red", fontSize: 50}}/>
                                </Tooltip>
                            }
                        </div>
                    </div>
                </div>

                <div className="sidebar box">
                    <InfoLogUnit vsmVars={vsmVarsForDevToolComp}
                                 collapseDevToolComp={collapseDevToolComp}
                                 setCollapseDevToolComp={setCollapseDevToolComp}
                                 infoLogConents={infoLogContents}
                    />
                </div>

                <div className="inform box">
                    <div className="">
                        <h2>{informContent}</h2>
                    </div>
                </div>

                <div className="content box">
                    <InputSheetUnit inputSheetFieldDetails={inputSheetFieldDetails}
                                    setUserSubmittedInfo={setUserSubmittedInfo}
                                    updateUserSubmittedInfo={updateUserSubmittedInfo}
                                    sendSubmit={sendSubmitToVsm} sendCancel={sendCancelToVsm}
                                    userSubmittedInfo={userSubmittedInfo}
                    />
                </div>

            </div>
        </div>
    );
}

export default App;
