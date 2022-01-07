import GenerateFields from "./inputAllFields";
import {Col, Row} from "react-bootstrap";
import Button from "react-bootstrap/Button";
import React from "react";

function InputSheetUnit(props) {

    return (
        <Row>
            <form style={{
                height: "200 px",
                backgroundColor: "silver"
            }}>
                <fieldset>
                    {(props.formContents && (props.formContents.action === "REQUEST"))}
                    <GenerateFields inputValue={props.inputValue} formContents={props.formContents}/>

                    {
                        (props.formContents && (props.formContents.action === "SUCCESSFULSEND")) &&
                        <div>
                            <h2 style={{color: "green"}}>Successfully posted!</h2>
                        </div>
                    }
                </fieldset>
                {
                    (props.formContents && (props.formContents.action === "REQUEST")) &&
                    <Row style={{
                        margin: "10px"
                    }}>
                        <Row xs={2}>
                            <Button as={Col} variant="primary" onClick={props.sendSubmit}> Submit</Button>
                            <Button as={Col} variant="secondary" onClick={props.sendCancel}> Cancel</Button>
                        </Row>
                    </Row>
                }
                {
                    !(props.formContents && (props.formContents.action === "REQUEST")) &&
                    <div>
                        <p style={{color: "white"}}>No active requests.</p>
                    </div>
                }
            </form>
        </Row>
    )
}

export default InputSheetUnit;