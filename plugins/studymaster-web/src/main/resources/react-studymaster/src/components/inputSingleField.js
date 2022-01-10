import {Col, Row} from "react-bootstrap";
import React from "react";

function GenerateInputFieldWithType(updateUserSubmittedInfo, formContents, currIdx) {
    let variable = formContents.variable[currIdx];
    if (formContents.type[currIdx] === "text") {
        return (
            <Row style={{
                marginTop: "20px",
                marginBottom: "20px"
            }}>
                <Col xs={2}>
                    <label> {formContents.variable[currIdx]} </label>
                </Col>
                <Col>
                    <input type="text" name={variable} placeholder={formContents.options[currIdx]} id={variable}
                           onChange={e => updateUserSubmittedInfo(variable, e.target.value)}/>
                </Col>
            </Row>
        )
    } else if (formContents.type[currIdx] === "number") {
        return (
            <Row style={{
                marginTop: "20px",
                marginBottom: "20px"
            }}>
                <Col xs={2}>
                    <label> {formContents.variable[currIdx]} </label>
                </Col>
                <Col>
                    <input type="number" min={1} max={300} style={{"width": "150px"}} name={variable}
                           placeholder={formContents.options[currIdx]} id={variable}
                           onChange={e => updateUserSubmittedInfo(variable, e.target.value)}/>
                </Col>
            </Row>
        )
    } else if (formContents.type[currIdx] === "radio") {
        let values = formContents.options[currIdx].split(',');
        return (
            <Row style={{
                marginTop: "20px",
                marginBottom: "20px"
            }}>
                <Col xs={2}>
                    <label> {formContents.variable[currIdx]} </label>
                </Col>
                <Col>
                    <Row>
                        {values.map((option) =>
                            <Col xs={1}>
                                <input type="radio" id={option} name={variable} value={option}/>
                                <label style={{"marginLeft": "10px"}}> {option} </label>
                            </Col>
                        )}
                    </Row>
                </Col>
            </Row>
        )
    } else if (formContents.type[currIdx] === "checkbox") {
        return (
            <Row style={{
                marginTop: "20px",
                marginBottom: "20px"
            }}>
                <Col xs={2}>
                    <label> {formContents.variable[currIdx]} </label>
                </Col>
                <Col>
                    <input type="checkbox" id={variable} name={variable} value={formContents.options[currIdx]}/>
                    <label style={{"marginLeft": "10px"}}> {formContents.options[currIdx]} </label>
                </Col>
            </Row>
        )
    }
}

export default GenerateInputFieldWithType;