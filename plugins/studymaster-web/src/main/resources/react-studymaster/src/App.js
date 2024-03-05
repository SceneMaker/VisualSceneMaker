import React, {useEffect, useState} from 'react';
import './App.css';
import 'bootstrap/dist/css/bootstrap.min.css';
import InfoLogUnit from "./components/infoLogUnit";
import InputSheetUnit from "./components/inputSheetUnit";
import LinkIcon from '@mui/icons-material/Link';
import LinkOffIcon from '@mui/icons-material/LinkOff';
import {Tooltip} from "@material-ui/core";
import NavigateNextIcon from '@mui/icons-material/NavigateNext';
import Button from "@mui/material/Button";

function genTimeStamp() {
    let today = new Date();
    const pad = (n,s=2) => (`${new Array(s).fill(0)}${n}`).slice(-s);

    let ms = today.getMilliseconds();
    ms = (ms < 10? "0": "") + ms;
    ms = (ms < 100? "0": "") + ms;

    let timestamp = today.getFullYear() + '-' + pad(today.getMonth() + 1) + '-' + pad(today.getDate()) + "_" +
        pad(today.getHours()) + ":" + pad(today.getMinutes()) + ":" + pad(today.getSeconds()) + ":" + ms;
    return timestamp;
}

/**
 * Studymaster web app main page.
 *
 * @version 1.0
 * @author [Chirag Bhuvaneshwara](https://github.com/chiragbhuvaneshwara)
 **/
function App() {
    const proto = document.location.protocol === 'https:'? 'wss://':'ws://';
    const [webSocket, setWebSocket] = useState(null);
    const [vsmConnectionStatus, setVsmConnectionStatus] = useState(false);
    const [infoLogContents, setInfoLogContents] = useState({});
    const [informContent, setInformContent] = useState("");
    const [inputSheetFieldDetails, setInputSheetFieldDetails] = useState();
    const [collapseDevToolComp, setCollapseDevToolComp] = useState(true);
    const [dispProceedBtn, setDispProceedBtn] = useState(false);
    const [proceedBtnUid, setProceedBtnUid] = useState("");
    const [vsmVarsForDevToolComp, setVsmVarsForDevToolComp] = useState({});
    const [userSubmittedInfo, setUserSubmittedInfo] = useState({});

    const updateUserSubmittedInfo = (k, v) => {
        let items = {...userSubmittedInfo};
        items[k] = v;
        setUserSubmittedInfo(items);
    };


    useEffect(() => {

        let ws = new WebSocket(proto + document.location.host + '/ws');
        console.log("Setting up web socket...");

        ws.onopen = function () {
            setVsmConnectionStatus(true);
            clientAliveMessage();
            console.log("Connection initiated by server.")
            setVsmVarsForDevToolComp({});
            setInfoLogContents({});

        };

        ws.onclose = function () {
            setVsmConnectionStatus(false);
            setUserSubmittedInfo({});
            setInputSheetFieldDetails({});

            setDispProceedBtn(false);
            setProceedBtnUid("");
            let err_msg = "VSM is not in 'play' state anymore. Please press 'play' in VSM and refresh the page.";
            console.log(err_msg);
            setInformContent(err_msg);

        };

        ws.onmessage = function (msg) {
            const parts = msg.data.split('#');
            const command = parts[1];

            if (["REQUEST", "PROCEED", "INFORM", "UPDATE", "STATUS"].includes(command)) {
                if (command === "REQUEST") {
                    // This cmd is used to generate the input fields where the "study master" can input values.
                    // Ex: TextInput,SliderInput etc.
                    // cmd type in VSM: Blocking ==> All other execution in VSM scene is halted till the request is
                    // submitted.
                    let newInputSheetFieldDetails = {
                        action: command,
                        variable: parts[3].split(';'),
                        options: parts[4].split(';'),
                        type: parts[5].split(';'),
                        timestamp: parts[2],
                        vm_uid: parts[6]         // vm_uid ==> contains information about which thread needs to be
                                                 // unblocked in VSM
                    };
                    setInputSheetFieldDetails(newInputSheetFieldDetails);
                    let newObj = newInputSheetFieldDetails.variable.reduce((obj, key) => ({...obj, [key]: ""}), {})
                    setUserSubmittedInfo(newObj);
                    let currTS = genTimeStamp();
                    let newInfo = {};
                    newInfo[currTS] = [command, parts[5]];
                    setInfoLogContents(Object.assign(infoLogContents, newInfo));
                }

                if (command === "PROCEED") {
                    // This cmd is used to provide some information to the "study master" and it generates a button
                    // that must be clicked by the studymaster for the execution to continue.
                    // cmd type in VSM: Blocking ==> All other execution in VSM scene is halted till the request is
                    // submitted.
                    let currTS = genTimeStamp();
                    let newInfo = {};
                    newInfo[currTS] = [command, parts[4]];
                    setInformContent(parts[4]);
                    setDispProceedBtn(true);
                    setProceedBtnUid(parts[6]);
                    setInfoLogContents(Object.assign(infoLogContents, newInfo));
                }

                if (command === "INFORM") {
                    // This cmd is used to provide some information to the "study master" to read.
                    // cmd type in VSM: Non-blocking
                    let currTS = genTimeStamp();
                    let newInfo = {};
                    newInfo[currTS] = [command, parts[4]];
                    setInformContent(parts[4]);
                    setInfoLogContents(Object.assign(infoLogContents, newInfo));
                    setDispProceedBtn(false);
                }

                if (command === "UPDATE") {
                    // This cmd is used to display the updated state of all variables that are declared in VSM.
                    // This cmd is executed upon update to variables in VSM.
                    // cmd type in VSM: Non-blocking and is not connected to any node i.e. it works in the background.
                    let variable = parts[2];
                    let val = parts[3];
                    let newVarVal = {}
                    newVarVal[variable] = [val];
                    setVsmVarsForDevToolComp(Object.assign(vsmVarsForDevToolComp, newVarVal));
                }

                if (command === "STATUS") {
                    // This cmd is just a reply from the server to keep the web socket connection alive
                    // This concept is referred to as the "ping-pong" messages exchanged between client
                    // (studymaster GUI) and server (VSM) to keep the connection alive.
                    console.log()
                    ;
                }
            } else {
                console.log("Unknown command: " + command)
            }
            setVsmConnectionStatus(true);
        };
        setWebSocket(ws);
        document.title = "VSM StudyMaster";
        const link = document.querySelector("link[rel*='icon']") || document.createElement('link');
        link.type = 'image/x-icon';
        link.rel = 'shortcut icon';
        link.href = document.location.protocol +'scenemaker.dfki.de/images/scenemaker/logo.png';
        document.getElementsByTagName('head')[0].appendChild(link);

        return () => {
            ws.disconnect();
        }

        // below eslint comment is to disable a warning about not passing dependencies ==> we do not pass any dependencies
        // because we want the useEffect hook to be executed only once i.e. we set up only one webSocket for the current
        // client.

        // eslint-disable-next-line
    }, []);

    /**
     * Message from client (i.e. studymaster frontend) that client is alive
     */
    function clientAliveMessage() {
        // This message is sent from the client to keep the web socket connection alive
        // This concept is referred to as the "ping-pong" messages exchanged between the client
        // (studymaster GUI) and the server (VSM) to keep the connection alive.
        // console.log("Send client alive message to server");
        if (webSocket.readyState === WebSocket.OPEN) {
            webSocket.send(`VSMMessage#STATUS#alive`);
            setTimeout(clientAliveMessage, 100);
        }
    }

    function sendSubmitToVsm() {

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

                    let checkBoxState = userSubmittedInfo[variable];
                    if (checkBoxState === ""){
                        checkBoxState = " ";
                    }

                    webSocket.send(`VSMMessage#VAR#${variable}#${checkBoxState}`);
                }
            } else if (inputSheetFieldDetails.type[i] === "slider") {
                if (userSubmittedInfo.hasOwnProperty(variable)) {
                    webSocket.send(`VSMMessage#VAR#${variable}#${userSubmittedInfo[variable]}`);
                }
            }
        }

        // vm_uid ==> contains information about which thread needs to be unblocked in VSM
        webSocket.send(`VSMMessage#VAR#request_result#SUBMIT#` + inputSheetFieldDetails.vm_uid);
        setInputSheetFieldDetails({
            action: "SUCCESSFULSEND",
            timestamp: inputSheetFieldDetails.timestamp,
        })
        setUserSubmittedInfo(new Map());
    }

    function sendCancelToVsm() {
        webSocket.send(`VSMMessage#VAR#request_result#CANCEL#` + inputSheetFieldDetails.vm_uid);

        setInfoLogContents({});
        setUserSubmittedInfo({});
        setInputSheetFieldDetails({});
        setInformContent("");
        setDispProceedBtn(false);
        setProceedBtnUid("");
    }

    function sendProceedToVsm() {
        webSocket.send(`VSMMessage#VAR#request_result#PROCEED#` + proceedBtnUid);

        setInfoLogContents({});
        setUserSubmittedInfo({});
        setInputSheetFieldDetails({});
        setInformContent("");
        setDispProceedBtn(false);
        setProceedBtnUid("");
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
                                 infoLogContents={infoLogContents}
                    />
                </div>

                <div className="inform box">
                    <div className="flex-container">
                        <div className="item1">
                            <h2>{informContent}</h2>
                        </div>

                        {
                            (dispProceedBtn) &&
                            <div className="item2">
                                <div className="button-area">
                                    <Button variant="contained"
                                            onClick={() => {
                                                sendProceedToVsm();
                                            }}
                                    >
                                        <NavigateNextIcon/>
                                    </Button>
                                </div>
                            </div>
                        }
                    </div>
                </div>

                <div className="content box">
                    <InputSheetUnit inputSheetFieldDetails={inputSheetFieldDetails}
                                    setUserSubmittedInfo={setUserSubmittedInfo}
                                    updateUserSubmittedInfo={updateUserSubmittedInfo}
                                    sendSubmit={sendSubmitToVsm} sendCancel={sendCancelToVsm}
                                    userSubmittedInfo={userSubmittedInfo}
                                    infoLogContents={infoLogContents}
                                    webSocket={webSocket}
                    />
                </div>

            </div>
        </div>
    );
}

export default App;
