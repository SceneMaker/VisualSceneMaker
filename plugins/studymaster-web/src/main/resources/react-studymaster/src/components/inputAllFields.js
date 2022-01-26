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

    const generateKey = (pre) => {
        return `${ pre }_${ new Date().getTime() }`;
    }

    return (
        <div>
            {fieldsForUserForm.map((field) =>
                    <div
                        // key={generateKey(field.name)}
                    >
                        {field}
                    </div>
                )
                }
        </div>
    )
}

export default GenerateFields;