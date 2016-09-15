package de.dfki.vsm.runtime.values;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.util.TextFormat;
import org.apache.commons.lang3.StringEscapeUtils;

/**
 * @author Not me
 */
public class StringValue extends AbstractValue {

    private final String mValue;

    public StringValue(final String value) {
        mValue = value;
    }

    @Override
    public Type getType() {
        return Type.STRING;
    }

    @Override
    public String getAbstractSyntax() {
        return "StringValue(" + mValue + ")";
    }

    @Override
    public String getConcreteSyntax() {
        return "\"" + StringEscapeUtils.escapeJava(mValue) + "\"";
    }

    @Override

    public String getFormattedSyntax() {
        return TextFormat.formatConstantStringLiteral("\"" + StringEscapeUtils.escapeJava(mValue) + "\"");
    }

    @Override
    public String getValue() {
        return mValue;
    }

    @Override
    public StringValue getCopy() {
        return new StringValue(mValue);
    }

    @Override
    public boolean equalsValue(AbstractValue value) {
        if (value.getType() == Type.STRING) {
            return mValue.equals(((StringValue) value).mValue);
        }

        return false;
    }
}
