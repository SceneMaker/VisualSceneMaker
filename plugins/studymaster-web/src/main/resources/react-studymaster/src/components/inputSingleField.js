import {Col, Row} from "react-bootstrap";
import React from "react";

function GenerateInputFieldWithType(inputValue, formContents, props) {
    let variable = formContents.variable[props];
    if (formContents.type[props] === "text") {
        return (
            <Row style={{
                marginTop: "20px",
                marginBottom: "20px"
            }}>
                <Col xs={2}>
                    <label> {formContents.variable[props]} </label>
                </Col>
                <Col>
                    <input type="text" name={variable} placeholder={formContents.options[props]} id={variable}
                           onChange={e => inputValue.set(variable, e.target.value)}/>
                </Col>
            </Row>
        )
    } else if (formContents.type[props] === "number") {
        return (
            <Row style={{
                marginTop: "20px",
                marginBottom: "20px"
            }}>
                <Col xs={2}>
                    <label> {formContents.variable[props]} </label>
                </Col>
                <Col>
                    <input type="number" min={1} max={300} style={{"width": "150px"}} name={variable} placeholder={formContents.options[props]} id={variable}
                           onChange={e => inputValue.set(variable, e.target.value)}/>
                </Col>
            </Row>
        )
    } else if (formContents.type[props] === "radio") {
        let values = formContents.options[props].split(',');
        return (
            <Row style={{
                marginTop: "20px",
                marginBottom: "20px"
            }}>
                <Col xs={2}>
                    <label> {formContents.variable[props]} </label>
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
    } else if (formContents.type[props] === "checkbox") {
        return (
            <Row style={{
                marginTop: "20px",
                marginBottom: "20px"
            }}>
                <Col xs={2}>
                    <label> {formContents.variable[props]} </label>
                </Col>
                <Col>
                    <input type="checkbox" id={variable} name={variable} value={formContents.options[props]}/>
                    <label style={{"marginLeft": "10px"}}> {formContents.options[props]} </label>
                </Col>
            </Row>
        )
    }
}

export default GenerateInputFieldWithType;