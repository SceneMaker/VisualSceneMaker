// import {Row} from "react-bootstrap";
// import React from "react";
// import {FormHelperText, TextField} from "@mui/material";
// import RadioGroup from '@mui/material/RadioGroup';
// import FormControlLabel from '@mui/material/FormControlLabel';
// import FormControl from '@mui/material/FormControl';
// import {Checkbox} from "@material-ui/core";
// import {Form} from "react-bootstrap";
//
// function GenerateInputFieldWithType(props, updateUserSubmittedInfo, formContents, currIdx, error) {
//
//
//     let variable = formContents.variable[currIdx];
//
//     // useEffect(() => {
//     //     if (formContents.type[currIdx] === "checkbox") {
//     //         updateUserSubmittedInfo(variable, false);
//     //     }
//     // }, [])
//
//     if (formContents.type[currIdx] === "text") {
//         return (
//             <Row style={{
//                 marginTop: "30px",
//                 marginBottom: "30px"
//             }}>
//                 <label> {formContents.variable[currIdx]} </label>
//                 <TextField variant="standard" name={variable}
//                            label={formContents.options[currIdx]}
//                            value={props.userSubmittedInfo[variable]}
//                            id={variable}
//                            onChange={e => {
//                                updateUserSubmittedInfo(variable, e.target.value);
//                            }}
//                            {...(error[variable] && {
//                                error: true,
//                                helperText: error[variable]
//                            })}
//                 />
//             </Row>
//         )
//     } else if (formContents.type[currIdx] === "number") {
//         return (
//             <Row style={{
//                 marginTop: "30px",
//                 marginBottom: "30px"
//             }}>
//                 <label> {formContents.variable[currIdx]} </label>
//                 <TextField
//                     type="number" style={{"width": "150px"}} name={variable}
//                     InputProps={{inputProps: {min: 0, max: 1000}}}
//                     label={formContents.options[currIdx]}
//                     id={variable}
//                     value={props.userSubmittedInfo[variable]}
//                     onChange={e => updateUserSubmittedInfo(variable, e.target.value)}
//                     {...(error[variable] && {
//                         error: true,
//                         helperText: error[variable]
//                     })}
//                 />
//             </Row>
//         )
//     } else if (formContents.type[currIdx] === "radio") {
//         let values = formContents.options[currIdx].split(',');
//         return (
//             <Row style={{
//                 marginTop: "30px",
//                 marginBottom: "30px"
//             }}>
//                 <label> {formContents.variable[currIdx]} </label>
//                 <Row>
//                     <FormControl
//                         error={error[variable] !== undefined}
//                     >
//                         <RadioGroup
//                             aria-label={formContents.variable[currIdx]}
//                             name="radio-buttons-group"
//                         >
//
//                             {values.map((option) =>
//                                     <Form.Check
//                                         key={option}
//                                         name={"group1"}
//                                         type={"radio"}
//                                         label={option}
//                                         value={option}
//                                         onChange={e => {
//                                             updateUserSubmittedInfo(variable, e.target.value);
//                                             // console.log(e.target.value);
//                                         }}
//                                     />
//
//
//
//                                 // <FormControlLabel key={Math.random().toString(36).substr(2, 9)}
//                                 //                   id={option}
//                                 //                   value={option} control={
//                                 //     <Radio sx={{
//                                 //         'm': 3,
//                                 //         '&:hover': {
//                                 //             bgcolor: 'transparent',
//                                 //         },
//                                 //     }}
//                                 //            color="default"/>} label={option}
//                                 //                   onChange={e => {
//                                 //                       updateUserSubmittedInfo(variable, e.target.value);
//                                 //                   }}/>
//                             )}
//                             <FormHelperText>{error[variable]}</FormHelperText>
//                         </RadioGroup>
//                     </FormControl>
//
//                 </Row>
//             </Row>
//         )
//     } else if (formContents.type[currIdx] === "checkbox") {
//         let values = formContents.options[currIdx].split(',');
//         // const checkBoxState = Object.fromEntries(new Map([
//         //     [values], Array(values.length).fill(false)
//         // ]));
//         let checkBoxState = values.reduce((a, v) => ({ ...a, [v]: false}), {}) ;
//
//         function updateCheckBoxState(e) {
//             checkBoxState = {...checkBoxState,
//                 [e.target.name]:e.target.checked}
//             console.log(checkBoxState);
//         }
//
//         return (
//             <Row style={{
//                 marginTop: "30px",
//                 marginBottom: "30px"
//             }}>
//                 <label> {formContents.variable[currIdx]} </label>
//
//                 <FormControl
//                     error={error[variable] !== undefined}
//                 >
//                     {values.map((option) =>
//                         <FormControlLabel
//                             key={option}
//                             label={option}
//                             control={
//                                 <Checkbox
//                                     defaultValue={false}
//                                     name={option}
//                                     // checked={option}
//                                     color="default"
//                                     onChange={e => {
//                                         updateUserSubmittedInfo(variable, updateCheckBoxState(e));
//                                         // updateUserSubmittedInfo(variable, checkBoxState);
//                                     }}
//                                 />
//                             }
//                         />
//                     )}
//                     <FormHelperText>{error[variable]}</FormHelperText>
//                 </FormControl>
//             </Row>
//         )
//     }
// }
//
// export default GenerateInputFieldWithType;