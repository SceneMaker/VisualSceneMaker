package de.dfki.vsm.model.sceneflow.language.definition;

import de.dfki.vsm.model.sceneflow.language.LanguageObject;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import org.w3c.dom.Element;
import java.lang.reflect.Field;
import java.util.ArrayList;

/**
 * @author Gregor Mehlmann
 */
public final class FunctionDefinition extends LanguageObject implements Comparable<FunctionDefinition> {

    private String mName;
    private String mClazz;
    private String mMethod;
    private ArrayList<ArgumentDefinition> mArgList
            = new ArrayList<>();

    public FunctionDefinition() {
        mName = new String();
        mClazz = new String();
        mMethod = new String();
    }

    public FunctionDefinition(
            final String name,
            final String clazz,
            final String method) {
        mName = name;
        mClazz = clazz;
        mMethod = method;
    }

    public FunctionDefinition(
            final String name,
            final String clazz,
            final String method,
            final ArrayList argList) {
        mName = name;
        mClazz = clazz;
        mMethod = method;
        mArgList = argList;
    }

    public final String getName() {
        return mName;
    }

    public final void setName(final String value) {
        mName = value;
    }

    public final String getClazz() {
        return mClazz;
    }

    public final void setClazz(final String value) {
        mClazz = value;
    }

    public final String getMethod() {
        return mMethod;
    }

    public final void setMethod(final String value) {
        mMethod = value;
    }

    public final ArrayList getArgList() {
        return mArgList;
    }

    public final void setArgList(final ArrayList value) {
        mArgList = value;
    }

    public final int getArgCount() {
        return mArgList.size();
    }

    public final ArrayList getCopyOfParamList() {
        final ArrayList copy = new ArrayList();
        for (ArgumentDefinition arg : mArgList) {
            copy.add(arg.getCopy());
        }
        return copy;
    }

    public final boolean addArg(final ArgumentDefinition value) {
        return mArgList.add(value);
    }

    public final ArgumentDefinition getArgAt(final int index) {
        return mArgList.get(index);
    }

    @Override
    public final String getAbstractSyntax() {
        String desc = "";

        for (int i = 0; i < mArgList.size(); i++) {
            desc += mArgList.get(i).getAbstractSyntax();

            if (i != mArgList.size() - 1) {
                desc += ",";
            }
        }

        return "FunctionDefinition(" + mName + "(" + desc + "):" + mClazz + "." + mMethod + ")";
    }

    @Override
    public String getConcreteSyntax() {
        String desc = "";

        for (int i = 0; i < mArgList.size(); i++) {
            desc += mArgList.get(i).getConcreteSyntax();

            if (i != mArgList.size() - 1) {
                desc += ",";
            }
        }

        return mName + "( " + desc + " ):" + mClazz + "." + mMethod;
    }

    @Override
    public final String getFormattedSyntax() {
        return getConcreteSyntax();
    }

    @Override
    public FunctionDefinition getCopy() {
        return new FunctionDefinition(mName, mClazz, mMethod, getCopyOfParamList());
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) {
        out.println("<FunctionDefinition name=\"" + mName + "\" classname =\"" + mClazz + "\" method=\"" + mMethod
                + "\">").push();

        for (int i = 0; i < mArgList.size(); i++) {
            mArgList.get(i).writeXML(out);
        }

        out.pop().println("</FunctionDefinition>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mMethod = element.getAttribute("method");
        mClazz = element.getAttribute("classname");
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(Element element) throws XMLParseError {
                ArgumentDefinition var = new ArgumentDefinition();

                var.parseXML(element);
                mArgList.add(var);
            }
        });
    }

    public final String getParamPrettyPrint() {
        String desc = "(";

        for (int i = 0; i < mArgList.size(); i++) {
            desc += mArgList.get(i).getPrettyType();

            if (i != mArgList.size() - 1) {
                desc += ", ";
            }
        }

        return desc + ")";
    }

    public final boolean isValidClass() {
        boolean isClass = true;
        boolean isObject = true;

        try {
            final Class clazz = Class.forName(mClazz);
        } catch (ClassNotFoundException exc) {
            isClass = false;
        }

        try {
            final int dot = mClazz.lastIndexOf('.');
            final String clazzName = mClazz.substring(0, dot);
            final String fieldName = mClazz.substring(dot + 1);
            //
            final Class clazz = Class.forName(clazzName);
            final Field field = clazz.getField(fieldName);
            final Class type = field.getType();

        } catch (ClassNotFoundException |
                NoSuchFieldException |
                SecurityException exc) {
            isObject = false;
        }
        return isClass || isObject;
    }

    @Override
    public final int compareTo(final FunctionDefinition other) {
        return mName.compareTo(other.getName());
    }
}
