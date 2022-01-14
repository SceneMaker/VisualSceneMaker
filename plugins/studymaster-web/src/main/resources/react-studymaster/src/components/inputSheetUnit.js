import GenerateFields from "./inputAllFields";
import {Col, Container, Row} from "react-bootstrap";
import Button from "react-bootstrap/Button";
import React, {useState} from "react";

function InputSheetUnit(props) {

    const [formFillingErrors, setFormFillingErrors] = useState({});

    function validateForm() {

        if (props.userSubmittedInfo.size === 0) {
            let unfilledFields = [...props.inputSheetFieldDetails.variable].sort();
            let errors = {}
            unfilledFields.forEach(function (unfilledField) {
                    errors[unfilledField] = "Please fill " + unfilledField;
                }
            );
            setFormFillingErrors(errors);
            return false;
        } else if ([...props.userSubmittedInfo.keys()].length > 0) {

            let reqdVars = [];
            console.log(props.inputSheetFieldDetails.variable.length);
            for (let i = 0; i < props.inputSheetFieldDetails.variable.length; i++) {
                console.log(props.inputSheetFieldDetails.type[i])
                if (props.inputSheetFieldDetails.type[i] !== "checkbox") {
                    reqdVars.push(props.inputSheetFieldDetails.variable[i])
                }
            }

            console.log(reqdVars, props.inputSheetFieldDetails);
            let sortedReqdVars = [...reqdVars].sort();
            // let sortedReqdVars = [...props.inputSheetFieldDetails.variable].sort();
            let sortedFilledVars = [...props.userSubmittedInfo.keys()].sort();

            let unfilledFields = sortedReqdVars.filter(x => !sortedFilledVars.includes(x));


            let errors = {}
            unfilledFields.forEach(function (unfilledField) {
                    errors[unfilledField] = "Please fill " + unfilledField;
                }
            );
            setFormFillingErrors(errors);

            if (unfilledFields.length == 0) {
                return true;
            }
        }
    }

    function validateFormAndSubmit() {

        if (validateForm()) {
            props.sendSubmit();
        }
    }

    return (
        // <Container className="input-sheet-unit-divider">
            <div className="input-sheet-unit-divider">
                <div className="instruction-area">
                    <div className="">
                        {(props.infoLogContents && (props.infoLogContents.action === "INFORM")) &&
                        <h1>{props.infoLogContents.message}</h1>}
                    </div>
                </div>
                <form style={{
                    marginLeft: "1vw",
                }}>
                    <div className="in-form-fields">
                        <fieldset>
                            {(props.inputSheetFieldDetails && (props.inputSheetFieldDetails.action === "REQUEST"))}
                            <GenerateFields userSubmittedInfo={props.userSubmittedInfo}
                                            updateUserSubmittedInfo={props.updateUserSubmittedInfo}
                                            inputSheetFieldDetails={props.inputSheetFieldDetails}
                                            validateForm={validateForm}
                                            formFillingErrors={formFillingErrors}
                            />

                            {
                                (props.inputSheetFieldDetails && (props.inputSheetFieldDetails.action === "SUCCESSFULSEND")) &&
                                <div>
                                    <h2 style={{color: "green"}}>Successfully posted!</h2>
                                </div>
                            }
                        </fieldset>
                    </div>
                </form>
                <form>
                    <div className="in-form-buttons">
                        {
                            (props.inputSheetFieldDetails && (props.inputSheetFieldDetails.action === "REQUEST")) &&
                            <Row style={{
                                margin: "10px"
                            }}>
                                <Row xs={2}>
                                    <Button as={Col} variant="primary" onClick={validateFormAndSubmit}> Submit</Button>
                                    <Button as={Col} variant="secondary" onClick={props.sendCancel}> Cancel</Button>
                                </Row>
                            </Row>
                        }
                        {
                            !(props.inputSheetFieldDetails && (props.inputSheetFieldDetails.action === "REQUEST")) &&
                            <div>
                                <p style={{color: "white"}}>No active requests.</p>
                            </div>
                        }
                    </div>
                </form>
            </div>
        // </Container>
    )
}

export default InputSheetUnit;