import GenerateFields from "./inputAllFields";
import Button from '@mui/material/Button';
import React, {useState} from "react";
import {Grid, Stack} from "@mui/material";
import DeleteIcon from '@mui/icons-material/Delete';
import SendIcon from '@mui/icons-material/Send';

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
        } else if ([...Object.keys(props.userSubmittedInfo)].length > 0) {

            let reqdVars = [];
            for (let i = 0; i < props.inputSheetFieldDetails.variable.length; i++) {
                if (props.inputSheetFieldDetails.type[i] !== "checkbox") {
                    reqdVars.push(props.inputSheetFieldDetails.variable[i])
                }
            }

            let sortedReqdVars = [...reqdVars].sort();
            let sortedFilledVars = [...Object.keys(props.userSubmittedInfo)].sort();
            sortedFilledVars = sortedFilledVars.filter(x => props.userSubmittedInfo[x] !== "")

            let unfilledFields = sortedReqdVars.filter(x => !sortedFilledVars.includes(x));

            let errors = {}
            unfilledFields.forEach(function (unfilledField) {
                    errors[unfilledField] = "Please fill " + unfilledField;
                }
            );
            setFormFillingErrors(errors);

            if (unfilledFields.length === 0) {
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
        <div className="input-sheet-unit-divider">
            <form style={{
                marginLeft: "1vw",
            }}>
                <div className="in-form-fields">
                    <fieldset>
                        {(props.inputSheetFieldDetails && (props.inputSheetFieldDetails.action === "REQUEST"))}
                        <GenerateFields userSubmittedInfo={props.userSubmittedInfo}
                                        setUserSubmittedInfo={props.setUserSubmittedInfo}
                                        updateUserSubmittedInfo={props.updateUserSubmittedInfo}
                                        inputSheetFieldDetails={props.inputSheetFieldDetails}
                                        validateForm={validateForm}
                                        formFillingErrors={formFillingErrors}
                                        webSocket={props.webSocket}
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
                        <Grid container
                              spacing={0}
                              direction="column"
                              alignItems="center"
                              justifyContent="center"
                        >
                            <Grid item xs={2}>
                                <Stack direction="row" spacing={2} style={{textAlign: 'center'}}>
                                    <Button style={{
                                        borderRadius: 35,
                                        backgroundColor: "green",
                                        padding: "18px 36px",
                                        fontSize: "18px",
                                        width: "15vw"
                                    }} variant="contained"
                                            onClick={validateFormAndSubmit} startIcon={<SendIcon/>}> Submit</Button>
                                    <Button style={{
                                        borderRadius: 35,
                                        backgroundColor: "red",
                                        padding: "18px 36px",
                                        fontSize: "18px",
                                        width: "15vw"
                                    }} variant="contained" color="error"
                                            onClick={props.sendCancel} startIcon={<DeleteIcon/>}> Cancel</Button>
                                </Stack>
                            </Grid>
                        </Grid>

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
    )
}

export default InputSheetUnit;