import React from "react";
import GenerateInputFieldWithType from "./inputSingleField";


function GenerateFields(props) {

    let fieldsForUserForm = [];
    if (props.inputSheetFieldDetails && (props.inputSheetFieldDetails.action === "REQUEST")) {
        let i;
        for (i = 0; i < props.inputSheetFieldDetails.variable.length; i++) {
            let currField = GenerateInputFieldWithType(props, props.updateUserSubmittedInfo,
                                    props.inputSheetFieldDetails, i, props.formFillingErrors);
            fieldsForUserForm.push(currField);
        }
    }

    return (
        <div>
                {fieldsForUserForm}
        </div>
    )
}

export default GenerateFields;