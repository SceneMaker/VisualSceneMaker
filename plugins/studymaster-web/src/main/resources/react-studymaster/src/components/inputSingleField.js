import {Row} from "react-bootstrap";
import React, {useState} from "react";
import {FormHelperText, Stack, TextField} from "@mui/material";
import RadioGroup from '@mui/material/RadioGroup';
import FormControlLabel from '@mui/material/FormControlLabel';
import FormControl from '@mui/material/FormControl';
import {Checkbox, InputLabel, Slider} from "@material-ui/core";
import {Form} from "react-bootstrap";

function GenerateInputFieldWithType(props, updateUserSubmittedInfo, formContents, currIdx, error, webSocket) {


    let variable = formContents.variable[currIdx];

    const [sliderVal, setSliderVal] = React.useState(50);

    let values = formContents.options[currIdx].split(',');
    const marks = [
        {
            value: 0,
            label: values[0],
        },
        {
            value: 100,
            label: values[1],
        }
    ];

    function valuetext(value) {
        return `${value}`;
    }

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
        let checkBoxStateStr = " ";
        if (newCheckBoxStateArr.length > 0){
            checkBoxStateStr = newCheckBoxStateArr.join(",");
        }
        props.webSocket.send(`VSMMessage#VAR#${variable}#` + checkBoxStateStr);

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
                                   props.webSocket.send(`VSMMessage#VAR#${variable}#${e.target.value}`);
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
                        onChange={e => {
                            updateUserSubmittedInfo(variable, e.target.value);
                            props.webSocket.send(`VSMMessage#VAR#${variable}#${e.target.value}`);
                        }}
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
                                            props.webSocket.send(`VSMMessage#VAR#${variable}#${e.target.value}`);
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
                                            // let arr = Object.keys(checkBoxState).filter(k => checkBoxState[k] === true);
                                            // arr.indexOf(e.target.name) === -1 ? arr.push(e.target.name) : console.log("This item already exists");
                                            // props.webSocket.send(`VSMMessage#VAR#${variable}#` + arr.join(","));
                                            // console.log(`VSMMessage#VAR#${variable}#` + arr.join(","));
                                        }}
                                    />
                                }
                            />
                        )}
                        <FormHelperText>{error[variable]}</FormHelperText>
                    </FormControl>
                </Row>
            }
            {
                (formContents.type[currIdx] === "slider") &&
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

                        <Slider
                            value={sliderVal}
                            aria-label="Default" valueLabelDisplay="auto"
                            aria-label="Custom marks"
                            onChange={(e, newSliderVal) => {
                                updateUserSubmittedInfo(variable, newSliderVal);
                                setSliderVal(newSliderVal);
                                props.webSocket.send(`VSMMessage#VAR#${variable}#${newSliderVal}`);
                            }}
                            getAriaValueText={valuetext}
                            // step={10}
                            valueLabelDisplay="auto"
                            marks={marks}
                        />

                        <FormHelperText>{error[variable]}</FormHelperText>
                    </FormControl>
                </Row>
            }
        </div>
    )
}

export default GenerateInputFieldWithType;