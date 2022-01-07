import React from "react";
import GenerateInputFieldWithType from "./inputSingleField";


function GenerateFields(props) {
    let returnValue = [];
    if (props.formContents && (props.formContents.action === "REQUEST")){
        let i;
        for (i = 0; i < props.formContents.variable.length; i++) {
            let inputBoxes = GenerateInputFieldWithType(props.inputValue, props.formContents, i);
            returnValue.push(inputBoxes);
        }
    }

    return (
        <div>
            {returnValue}
        </div>
    )
}

export default GenerateFields;