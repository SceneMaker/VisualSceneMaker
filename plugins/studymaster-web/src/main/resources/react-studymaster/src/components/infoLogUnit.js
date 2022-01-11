import {Row} from "react-bootstrap";
import React from "react";

function InfoLogUnit(props) {
    return (
            <div className="" style={{
            }}>
                <div className="">
                    {(props.infoLogContents && (props.infoLogContents.action === "INFORM")) &&
                    <h1 style={{color: "white"}}>{props.infoLogContents.message}</h1>}
                </div>
            </div>
    )
}

export default InfoLogUnit;