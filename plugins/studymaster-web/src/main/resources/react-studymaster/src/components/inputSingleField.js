import {Row} from "react-bootstrap";
import React, {useState} from "react";
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

    function checkboxArrContainsObj(a, obj) {

        if (typeof a === "undefined") {
            return false;
        }

        let i = a.length;
        while (i--) {
            if (a[i] === obj) {
                return true;
            }
        }
        return false;
    }


    function updateCheckBoxAndUserInfo(variable, e) {

        let newCheckBoxState = {
            ...checkBoxState,
            [e.target.name]: e.target.checked,
        }

        setCheckBoxState(newCheckBoxState)

        let newCheckBoxStateArr = Object.keys(newCheckBoxState).filter(k => newCheckBoxState[k] === true);
        updateUserSubmittedInfo(variable, newCheckBoxStateArr);
    }

    return (
        <div>
            {
                (formContents.type[currIdx] === "text") &&
                <Row style={{
                    marginTop: "30px",
                    marginBottom: "30px"
                }}>
                    <InputLabel color='primary' focused={true} htmlFor={variable}
                                style={{
                                    marginBottom: "10px"
                                }}
                    >
                        {formContents.variable[currIdx]}
                    </InputLabel>
                    <TextField variant="standard" name={variable}
                               label={formContents.options[currIdx]}
                               value={props.userSubmittedInfo[variable] || ''}
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
                    <InputLabel color='primary' focused={true} htmlFor={variable}
                                style={{
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
                    <InputLabel color='primary' focused={true} htmlFor={variable}
                                style={{
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
                                        checked={props.userSubmittedInfo[variable] === option || false}
                                        onChange={e => {
                                            updateUserSubmittedInfo(variable, e.target.value);
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
                    <InputLabel color='primary' focused={true} htmlFor={variable}
                                style={{
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
                                        checked={checkboxArrContainsObj(props.userSubmittedInfo[variable], option)}
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