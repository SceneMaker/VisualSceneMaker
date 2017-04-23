package de.dfki.vsm.util.extensions.renderers;

import de.dfki.vsm.util.extensions.renderers.customcontrollers.numbertext.NumberSpinner;
import de.dfki.vsm.util.extensions.value.ValueRenderable;

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
        if(valueProperty.hasDefaultValue()){
            Integer defaultValue = (Integer) valueProperty.getDefaultValue();
            value = new BigDecimal(defaultValue);
        }
        control = new NumberSpinner(value, new BigDecimal(STEP));

    }

    @Override
    public String getValue() {
        return String.valueOf(value.intValue());
    }


}
