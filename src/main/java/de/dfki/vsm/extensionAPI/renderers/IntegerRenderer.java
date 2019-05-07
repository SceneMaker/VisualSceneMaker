package de.dfki.vsm.extensionAPI.renderers;

import de.dfki.vsm.extensionAPI.renderers.customcontrollers.numbertext.NumberSpinner;

import java.math.BigDecimal;

/**
 * Created by alvaro on 4/20/17.
 */
public class IntegerRenderer extends ValueRender {
    public static final int STEP = 1;
    private BigDecimal value;
    @Override
    public void render() {
        value = new BigDecimal(0);
        setDefaultValue();
        control = new NumberSpinner(value, new BigDecimal(STEP));

    }

    private void setDefaultValue() {
        if(valueProperty.hasDefaultValue()){
            Integer defaultValue = (Integer) valueProperty.getDefaultValue();
            value = new BigDecimal(defaultValue);
        }

    }

    @Override
    public String getValue() {
        value = getNumberSpinner().getNumber();
        return String.valueOf(value.intValue());
    }

    private NumberSpinner getNumberSpinner() {
        return (NumberSpinner)control;
    }


}
