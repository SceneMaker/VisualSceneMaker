import React, {useEffect, useState} from 'react';
import './App.css';
import 'bootstrap/dist/css/bootstrap.min.css';
import InfoLogUnit from "./components/infoLogUnit";
import InputSheetUnit from "./components/inputSheetUnit";
import LinkIcon from '@mui/icons-material/Link';
import LinkOffIcon from '@mui/icons-material/LinkOff';
import PendingIcon from '@mui/icons-material/Pending';
import {Tooltip} from "@material-ui/core";

//TODO set up scrollable information log

function genTimeStamp() {
    let today = new Date();
    let timestamp = today.getFullYear() + '-' + (today.getMonth() + 1) + '-' + today.getDate() +
        today.getHours() + ":" + today.getMinutes() + ":" + today.getSeconds();
    return timestamp;
}

function App() {
    const [vsmConnectionStatus, setVsmConnectionStatus] = useState(false);
    const [webSocket, setWebSocket] = useState(new WebSocket('ws://' + document.location.host + '/ws'));
    // const [infoLogContents, setInfoLogContents] = useState();
    const [infoLogContents, setInfoLogContents] = useState({});
    const [informContent, setInformContent] = useState("");
    const [inputSheetFieldDetails, setInputSheetFieldDetails] = useState();
    const [collapseDevToolComp, setCollapseDevToolComp] = useState(true);
    const [vsmVarsForDevToolComp, setVsmVarsForDevToolComp] = useState({});
    const [userSubmittedInfo, setUserSubmittedInfo] = useState(new Map());
    const updateUserSubmittedInfo = (k, v) => {
        if (typeof (v) !== "string") {
            setUserSubmittedInfo(new Map(userSubmittedInfo.set(k, v)));
        } else {
            if (v.length !== 0) {
                setUserSubmittedInfo(new Map(userSubmittedInfo.set(k, v)));
            } else {
                delete userSubmittedInfo.delete(k);
            }
        }

    }


    useEffect(() => {
        // let ws = new WebSocket('ws://' + document.location.host + '/ws');
        let ws = webSocket;

        // setVsmConnectionStatus();
        ws.onopen = function () {
            setVsmConnectionStatus(true);
        };
        ws.onclose = function (msg) {
            console.log(msg);
            setVsmConnectionStatus(false);
        };
        ws.onmessage = function (msg) {

            console.log(msg);
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

                let currTS = genTimeStamp();

                let newInfo = {};
                newInfo[currTS] = parts[4];
                setInformContent(parts[4]);
                // console.log(newInfo)
                setInfoLogContents(Object.assign(infoLogContents, newInfo));
                // console.log(infoLogContents);
            }

            if (command === "UPDATE") {
                let variable = parts[2];
                let val = parts[3];
                let newVarVal = {}
                newVarVal[variable] = val;

                setVsmVarsForDevToolComp(Object.assign(vsmVarsForDevToolComp, newVarVal));
            }

            setVsmConnectionStatus(true);
        };
        setWebSocket(ws);
        document.title = "VSM StudyMaster";
        var link = document.querySelector("link[rel*='icon']") || document.createElement('link');
        link.type = 'image/x-icon';
        link.rel = 'shortcut icon';
        link.href = 'http://scenemaker.dfki.de/images/scenemaker/logo.png';
        document.getElementsByTagName('head')[0].appendChild(link);
    }, []);

    function timeout(delay) {
        return new Promise(res => setTimeout(res, delay));
    }

    function sendSubmitToVsm() {
        let areAllFieldsSet = true;

        if (areAllFieldsSet) {

            for (let i = 0; i < inputSheetFieldDetails.variable.length; i++) {
                let variable = inputSheetFieldDetails.variable[i];
                if (inputSheetFieldDetails.type[i] === "radio") {
                    if (userSubmittedInfo.has(variable)) {
                        webSocket.send(`VSMMessage#VAR#${variable}#${userSubmittedInfo.get(variable)}`);
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
                    if (userSubmittedInfo.has(variable)) {
                        webSocket.send(`VSMMessage#VAR#${variable}#${userSubmittedInfo.get(variable)}`);
                    }
                }
            }

            webSocket.send(`VSMMessage#VAR#request_result#SUBMIT`);
            setInputSheetFieldDetails({
                action: "SUCCESSFULSEND",
                timestamp: inputSheetFieldDetails.timestamp,
            })
            setUserSubmittedInfo(new Map());
            // console.log("Reset")
        } else {
            window.location.reload();
        }
    }

    function sendCancelToVsm() {
        webSocket.send(`VSMMessage#VAR#request_result#CANCEL`);
        // Resetting form to empty
        window.location.reload();
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
                    <InfoLogUnit vsmVars={vsmVarsForDevToolComp} collapseDevToolComp={collapseDevToolComp}
                                 setCollapseDevToolComp={setCollapseDevToolComp}
                                 infoLogConents={infoLogContents}
                    />
                </div>

                <div className="inform box">

                    <div className="">
                        {/*<h2>{infoLogContents[Object.keys(infoLogContents)[Object.keys(infoLogContents).length-1]]}</h2>*/}
                        <h2>{informContent}</h2>
                    </div>
                </div>

                <div className="content box">
                    <InputSheetUnit inputSheetFieldDetails={inputSheetFieldDetails}
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
