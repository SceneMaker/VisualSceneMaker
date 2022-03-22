import React, {useState} from "react";
import DeveloperModeIcon from '@mui/icons-material/DeveloperMode';
import {Collapse, Tooltip} from "@material-ui/core";
import BasicTable from "./utils/basicTable";
import DownloadIcon from '@mui/icons-material/Download';
import Button from "@mui/material/Button";


function genTimeStamp() {
    let today = new Date();
    const pad = (n, s = 2) => (`${new Array(s).fill(0)}${n}`).slice(-s);

    let ms = today.getMilliseconds();
    ms = (ms < 10 ? "0" : "") + ms;
    ms = (ms < 100 ? "0" : "") + ms;

    let timestamp = today.getFullYear() + '-' + pad(today.getMonth() + 1) + '-' + pad(today.getDate()) + "_" +
        pad(today.getHours()) + ":" + pad(today.getMinutes()) + ":" + pad(today.getSeconds()) + ":" + ms;
    return timestamp;
};

const InfoLogUnit = (props) => {
    const [open, setOpen] = useState(false);

    return (
        <div className="sidebar-divider">
            <div className="button-area ">
                <Button
                    variant="contained"
                    onClick={() => {
                        props.setCollapseDevToolComp(!props.collapseDevToolComp);
                        setOpen(!open);
                    }}
                    aria-controls="example-collapse-text"
                    aria-expanded={open}
                >
                    {
                        (open) &&
                        <div>
                            <DeveloperModeIcon/> Developer Tools
                        </div>
                    }
                    {
                        (!open) &&
                        <Tooltip disableFocusListener title="Developer Tools">
                            <DeveloperModeIcon/>
                        </Tooltip>

                    }
                </Button>
            </div>

            <div className="variable-list">
                <Collapse in={open} dimension="width">
                    <hr/>

                    <div>
                        <h3>VSM Variable List</h3>
                        <hr/>
                        <div className={open ? "logbox" : ""} style={{height: '35vh', color: 'white'}}>

                            <BasicTable
                                colNames={["Variables", "Value"]} colVals={props.vsmVars}/>
                        </div>
                    </div>

                </Collapse>
            </div>


            <div className="log">
                <Collapse in={open} dimension="width">
                    <hr/>

                    <div>
                        <div className="flex-container">
                            <div className="item1">
                                <h3>Log history</h3>
                            </div>
                            <div className="item2">
                                <Button
                                    variant="secondary"
                                    onClick={() => {
                                        console.log("Downloading...");
                                        const myData = props.infoLogContents;
                                        const fileName = "StudyMasterDebugLog" + genTimeStamp();
                                        const header = ["Timestamp", "cmdType", "cmdContent"];
                                        let csvData = [];
                                        Object.keys(myData).forEach(
                                            (k) => {
                                                csvData.push([k, myData[k].join(',')].join(','))
                                            }
                                        )
                                        const csv = [
                                            header.join(','),
                                            ...csvData
                                        ].join('\r\n');
                                        const blob = new Blob([csv], {type: 'application/csv'});
                                        const href = URL.createObjectURL(blob);
                                        const link = document.createElement('a');
                                        link.href = href;
                                        link.download = fileName + ".csv";
                                        document.body.appendChild(link);
                                        link.click();
                                        document.body.removeChild(link);
                                    }}
                                >
                                    <DownloadIcon/> Download logs
                                </Button>
                            </div>
                        </div>

                        <hr/>
                        <div className={open ? "logbox" : ""} style={{height: '35vh', color: 'white'}}>

                            <BasicTable
                                colNames={["Timestamp", "cmdType", "cmdContent"]} colVals={props.infoLogContents}/>

                        </div>
                    </div>
                </Collapse>
            </div>

        </div>

    )
};

export default InfoLogUnit;