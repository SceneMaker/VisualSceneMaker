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
    const [webSocket, setWebSocket] = useState(new WebSocket('ws://' + document.location.host + '/ws'));
    const [infoLogContents, setInfoLogContents] = useState({});
    const [informContent, setInformContent] = useState("");
    const [inputSheetFieldDetails, setInputSheetFieldDetails] = useState();
    const [collapseDevToolComp, setCollapseDevToolComp] = useState(true);
    const [vsmVarsForDevToolComp, setVsmVarsForDevToolComp] = useState({});
    const [userSubmittedInfo, setUserSubmittedInfo] = useState({});


    const updateUserSubmittedInfo = (k, v) => {
        // console.log(v.target.name);
        // 1. Make a shallow copy of the items
        let items = {...userSubmittedInfo};
        // 4. Put it back into our array. N.B. we *are* mutating the array here, but that's why we made a copy first
        items[k] = v;
        // 5. Set the state to our new copy
        setUserSubmittedInfo(items);
        // if (typeof (v) !== "string") {
        //     setUserSubmittedInfo(new Map(userSubmittedInfo.set(k, v)));
        // } else {
        //     if (v.length !== 0) {
        //         setUserSubmittedInfo(new Map(userSubmittedInfo.set(k, v)));
        //     } else {
        //         delete userSubmittedInfo.delete(k);
        //     }
        // }

    }


    useEffect(() => {
        let ws = webSocket;

        ws.onopen = function () {
            setVsmConnectionStatus(true);
        };
        ws.onclose = function (msg) {
            setVsmConnectionStatus(false);
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

                // setInputSheetFieldDetails({
                //     action: command,
                //     variable: parts[3].split(';'),
                //     options: parts[4].split(';'),
                //     type: parts[5].split(';'),
                //     timestamp: parts[2]
                // });

                setInputSheetFieldDetails(newInputSheetFieldDetails);


                // let idxCheckBox = undefined;
                // for (let i = 0; i < newInputSheetFieldDetails.variable.length; i++) {
                //     if (newInputSheetFieldDetails.type[i] === "checkbox"){
                //         idxCheckBox = i;
                //     }
                // }
                //
                // let values = newInputSheetFieldDetails.options[idxCheckBox].split(',');
                // setCheckBoxState(values.reduce((a, v) => ({...a, [v]: false}), {}));
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
        setWebSocket(ws);
        document.title = "VSM StudyMaster";
        var link = document.querySelector("link[rel*='icon']") || document.createElement('link');
        link.type = 'image/x-icon';
        link.rel = 'shortcut icon';
        link.href = 'http://scenemaker.dfki.de/images/scenemaker/logo.png';
        document.getElementsByTagName('head')[0].appendChild(link);
    }, []);

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
        // Resetting form to empty
        // for (let i = 0; i < inputSheetFieldDetails.variable.length; i++) {
        //     let variable = inputSheetFieldDetails.variable[i];
        //     if (inputSheetFieldDetails.type[i] === "radio") {
        //         if (userSubmittedInfo.hasOwnProperty(variable)) {
        //             setUserSubmittedInfo(new Map(userSubmittedInfo.set(variable, "")));
        //         }
        //     } else if (inputSheetFieldDetails.type[i] === "text") {
        //         if (userSubmittedInfo.hasOwnProperty(variable)) {
        //             setUserSubmittedInfo(new Map(userSubmittedInfo.set(variable, "")));
        //         }
        //     } else if (inputSheetFieldDetails.type[i] === "number") {
        //         if (userSubmittedInfo.hasOwnProperty(variable)) {
        //             setUserSubmittedInfo(new Map(userSubmittedInfo.set(variable, "")));
        //         }
        //     } else if (inputSheetFieldDetails.type[i] === "checkbox") {
        //         if (userSubmittedInfo.hasOwnProperty(variable)) {
        //             setUserSubmittedInfo(new Map(userSubmittedInfo.set(variable, "")));
        //         }
        //     }
        // }
        // setUserSubmittedInfo(new Map(userSubmittedInfo.set("name", "")));
        // updateUserSubmittedInfo("surname", "sdfsd");
        // updateUserSubmittedInfo("name", "");

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
                    console.log(variable)

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
