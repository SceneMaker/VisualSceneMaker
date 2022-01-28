import {Button, Row} from "react-bootstrap";
import React, {useState} from "react";
import DeveloperModeIcon from '@mui/icons-material/DeveloperMode';
import {Card, Collapse, Tooltip} from "@material-ui/core";
import BasicTable from "./utils/basicTable";

const InfoLogUnit = (props) => {
    const [open, setOpen] = useState(false);

    return (
        <div className="sidebar-divider">
            <div className="button-area">
                <Button
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
                <div className={open ? "logbox" : ""} style={{minHeight: '35vh', color: 'white'}}>
                    <Collapse in={open} dimension="width">
                        <div id="example-collapse-text">
                            VSM Variable List (Work in Progress)
                            <hr/>
                            <BasicTable
                                colNames={["Variables", "Value"]} colVals={props.vsmVars}/>
                        </div>
                    </Collapse>
                </div>
            </div>

            <div className="log">
                <div className={open ? "logbox" : ""} style={{minHeight: '35vh', color: 'white'}}>
                    <Collapse in={open} dimension="width">
                        <div id="example-collapse-text">
                            Log history (Work in Progress)
                            <hr/>

                            <BasicTable
                                colNames={["Timestamp", "InformCmd"]} colVals={props.infoLogConents}/>

                        </div>
                    </Collapse>
                </div>
            </div>

        </div>

    )
};

export default InfoLogUnit;