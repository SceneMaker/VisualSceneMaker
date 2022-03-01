import React from "react";
import GenerateInputFieldWithType from "./inputSingleField";


function GenerateFields(props) {

    let fieldsForUserForm = [];

    if (props.inputSheetFieldDetails && (props.inputSheetFieldDetails.action === "REQUEST")) {
        for (let i = 0; i < props.inputSheetFieldDetails.variable.length; i++) {
            let currField = GenerateInputFieldWithType(props, props.updateUserSubmittedInfo,
                props.inputSheetFieldDetails, i, props.formFillingErrors);
            fieldsForUserForm.push(currField);
        }
    }


    return (
        <div>
            {fieldsForUserForm.map((field, index) =>
                <div
                    key={index}
                >
                    {field}
                </div>
            )
            }
        </div>
    )
}

export default GenerateFields;