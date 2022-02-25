import {Row} from "react-bootstrap";
import React, {useEffect, useState} from "react";
import {FormHelperText, TextField} from "@mui/material";
import RadioGroup from '@mui/material/RadioGroup';
import FormControlLabel from '@mui/material/FormControlLabel';
import FormControl from '@mui/material/FormControl';
import {Checkbox, InputLabel} from "@material-ui/core";
import {Form} from "react-bootstrap";

function GenerateInputFieldWithType(props, updateUserSubmittedInfo, formContents, currIdx, error) {


    let variable = formContents.variable[currIdx];

    let values = formContents.options[currIdx].split(',');

    const [checkBoxState, setCheckBoxState] = useState(values.reduce((a, v) => ({...a, [v]: false}), {}));

    // Similar to componentDidMount and componentDidUpdate:
    // useEffect(() => {
    //     console.log(props.userSubmittedInfo);
    //     console.log(variable);
    //     // updateUserSubmittedInfo(variable, "");
    //     props.setUserSubmittedInfo({ ...props.userSubmittedInfo,
    //         [variable]: ""});
    // }, []);

    function updateCheckBoxAndUserInfo(variable, e) {

        let newCheckBoxState = {
            ...checkBoxState,
            [e.target.name]: e.target.checked,
        }

        setCheckBoxState(newCheckBoxState)

        console.log(Object.keys(newCheckBoxState).filter(k => newCheckBoxState[k] === true));

        let newCheckBoxStateStr = Object.keys(newCheckBoxState).filter(k => newCheckBoxState[k] === true);
        // console.log( Object.values(newCheckBoxState).toString());
        // console.log(typeof (newCheckBoxStateStr));
        // console.log(newCheckBoxStateStr[4]);

        updateUserSubmittedInfo(variable, newCheckBoxStateStr);
    }

    return (
        <div>
            {
                (formContents.type[currIdx] === "text") &&
                <Row style={{
                    marginTop: "30px",
                    marginBottom: "30px"
                }}>
                    {/*<label> {formContents.variable[currIdx]} </label>*/}
                    <InputLabel color='primary' focused={true} htmlFor={variable}
                                style={{
                                    // marginTop: "30px",
                                    marginBottom: "10px"
                                }}
                    >
                        {formContents.variable[currIdx]}
                    </InputLabel>
                    <TextField variant="standard" name={variable}
                               label={formContents.options[currIdx]}
                               value={props.userSubmittedInfo[variable] || ''}
                        // style={{"width": "90%"}}
                               id={variable}
                               onChange={e => {
                                   updateUserSubmittedInfo(variable, e.target.value);
                               }}
                               {...(error[variable] && {
                                   error: true,
                                   helperText: error[variable]
                               })}
                    />
                </Row>
            }
            {
                (formContents.type[currIdx] === "number") &&
                <Row style={{
                    marginTop: "30px",
                    marginBottom: "30px"
                }}>
                    {/*<label> {formContents.variable[currIdx]} </label>*/}
                    <InputLabel color='primary' focused={true} htmlFor={variable}
                                style={{
                                    // marginTop: "30px",
                                    marginBottom: "10px"
                                }}
                    >
                        {formContents.variable[currIdx]}
                    </InputLabel>
                    <TextField
                        type="number" style={{"width": "150px"}} name={variable}
                        InputProps={{inputProps: {min: 0, max: 1000}}}
                        label={formContents.options[currIdx]}
                        id={variable}
                        value={props.userSubmittedInfo[variable] || ''}
                        onChange={e => updateUserSubmittedInfo(variable, e.target.value)}
                        {...(error[variable] && {
                            error: true,
                            helperText: error[variable]
                        })}
                    />
                </Row>
            }
            {
                (formContents.type[currIdx] === "radio") &&
                <Row style={{
                    marginTop: "30px",
                    marginBottom: "30px"
                }}>
                    {/*<label> {formContents.variable[currIdx]} </label>*/}
                    <InputLabel color='primary' focused={true} htmlFor={variable}
                                style={{
                                    // marginTop: "30px",
                                    marginBottom: "10px"
                                }}
                    >
                        {formContents.variable[currIdx]}
                    </InputLabel>
                    <Row>
                        <FormControl
                            error={error[variable] !== undefined}
                        >
                            <RadioGroup
                                aria-label={formContents.variable[currIdx]}
                                name="radio-buttons-group"
                            >

                                {values.map((option) =>
                                    <Form.Check
                                        key={option}
                                        name={"group1"}
                                        type={"radio"}
                                        label={option}
                                        value={option || ''}
                                        onChange={e => {
                                            updateUserSubmittedInfo(variable, e.target.value);
                                            // console.log(e.target.value);
                                        }}
                                    />
                                )}
                                <FormHelperText>{error[variable]}</FormHelperText>
                            </RadioGroup>
                        </FormControl>

                    </Row>
                </Row>
            }
            {
                (formContents.type[currIdx] === "checkbox") &&
                <Row style={{
                    marginTop: "30px",
                    marginBottom: "30px"
                }}>
                    {/*<label> {formContents.variable[currIdx]} </label>*/}
                    <InputLabel color='primary' focused={true} htmlFor={variable}
                                style={{
                                    // marginTop: "30px",
                                    marginBottom: "10px"
                                }}
                    >
                        {formContents.variable[currIdx]}
                    </InputLabel>

                    <FormControl
                        error={error[variable] !== undefined}
                    >
                        {values.map((option) =>
                            <FormControlLabel
                                key={option}
                                label={option}
                                control={
                                    <Checkbox
                                        defaultValue={false}
                                        name={option}
                                        color="default"
                                        onChange={e => {
                                            updateCheckBoxAndUserInfo(variable, e);
                                        }}
                                    />
                                }
                            />
                        )}
                        <FormHelperText>{error[variable]}</FormHelperText>
                    </FormControl>
                </Row>
            }
        </div>
    )
}

export default GenerateInputFieldWithType;