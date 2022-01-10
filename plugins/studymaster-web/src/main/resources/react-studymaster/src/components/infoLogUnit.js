import {Row} from "react-bootstrap";
import React from "react";

function InfoLogUnit(props) {
    return(
    <Row style={{
        minHeight: "400 px",
    }}>
        <div className="" style={{
            minHeight: "400 px",
            backgroundColor: "black"
        }}>
            <div className="h-100 d-inline-block">
                {(props.infoLogContents && (props.infoLogContents.action === "INFORM")) &&
                <h1 style={{color: "white"}}>{props.infoLogContents.message}</h1>}
            </div>
        </div>
    </Row>
    )
}

export default InfoLogUnit;