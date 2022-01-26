import {Button, Row} from "react-bootstrap";
import React, {useState} from "react";
import DeveloperModeIcon from '@mui/icons-material/DeveloperMode';
import {Card, Collapse} from "@material-ui/core";

function InfoLogUnit(props) {
    const [open, setOpen] = useState(false);
    const [vsmVars, setVsmVars] = useState({
        name: "Karla",
        gender: "f",
    });

    // var renderVsmVars = vsmVars.map((d) => <li key={d.name}>{d.name}</li>);

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
                        < DeveloperModeIcon/>
                    }
                </Button>
            </div>

            <div className="variable-list">
                {/*<Row>*/}
                    <div className={open ? "logbox" : ""} style={{minHeight: '35vh', color: 'white'}}>
                        <Collapse in={open} dimension="width">
                            <div id="example-collapse-text">
                                VSM Variable List (Work in Progress)
                                <hr/>
                                <div>
                                    {

                                        Object.keys(props.vsmVars).map((key, index) => (
                                            <p key={index}> {key} : {props.vsmVars[key]}</p>
                                        ))
                                    }
                                </div>
                            </div>
                        </Collapse>
                    </div>
                {/*</Row>*/}
            </div>

            <div className="log">
                {/*<Row>*/}
                    <div className={open ? "logbox" : ""} style={{minHeight: '35vh', color: 'white'}}>
                        <Collapse in={open} dimension="width">
                            <div id="example-collapse-text">
                                Log history (Work in Progress)
                                <hr/>

                            </div>
                        </Collapse>
                    </div>
                {/*</Row>*/}
            </div>

        </div>

    )
}

export default InfoLogUnit;