package de.dfki.vsm.model.sceneflow.glue.command.definition;

import de.dfki.vsm.model.sceneflow.glue.command.Definition;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import org.w3c.dom.Element;
import java.lang.reflect.Field;
import java.util.ArrayList;

/**
 * @author Gregor Mehlmann
 */
public class FunctionDefinition extends Definition implements  Comparable<FunctionDefinition> {

    private String mName;
    private String mClassName;
    private String mMethod;
    private ArrayList<ArgumentDefinition> mParamList;
    private boolean active = true;

    public FunctionDefinition() {
        mName = new String();
        mClassName = new String();
        mMethod = new String();
        mParamList = new ArrayList<ArgumentDefinition>();
    }

    public FunctionDefinition(String name, String className, String method) {
        mName = name;
        mClassName = className;
        mMethod = method;
        mParamList = new ArrayList<ArgumentDefinition>();
    }

    public FunctionDefinition(String name, String className, String method, ArrayList<ArgumentDefinition> paramList) {
        mName = name;
        mClassName = className;
        mMethod = method;
        mParamList = paramList;
    }

    public boolean isActive() {
        return active;
    }

    public void setActive(boolean active) {
        this.active = active;
    }

    public String getName() {
        return mName;
    }

    public void setName(String value) {
        mName = value;
    }

    public String getClassName() {
        return mClassName;
    }

    public void setClassName(String value) {
        mClassName = value;
    }

    public String getMethod() {
        return mMethod;
    }

    public void setMethod(String value) {
        mMethod = value;
    }

    public ArrayList<ArgumentDefinition> getParamList() {
        return mParamList;
    }

    public void setParamList(ArrayList<ArgumentDefinition> value) {
        mParamList = value;
    }

    public int getSizeOfParamList() {
        return mParamList.size();
    }

    public ArrayList<ArgumentDefinition> getCopyOfParamList() {
        ArrayList<ArgumentDefinition> copy = new ArrayList<ArgumentDefinition>();

        for (ArgumentDefinition param : mParamList) {
            copy.add(param.getCopy());
        }

        return copy;
    }

    public boolean addParam(ArgumentDefinition value) {
        return mParamList.add(value);
    }

    public ArgumentDefinition getParamAt(int index) {
        return mParamList.get(index);
    }

    @Override
    public String getAbstractSyntax() {
        String desc = "";

        for (int i = 0; i < mParamList.size(); i++) {
            desc += mParamList.get(i).getAbstractSyntax();

            if (i != mParamList.size() - 1) {
                desc += ",";
            }
        }

        return "UsrCmdDef(" + mName + "(" + desc + "):" + mClassName + "." + mMethod + ")";
    }

    @Override
    public String getConcreteSyntax() {
        String desc = "";

        for (int i = 0; i < mParamList.size(); i++) {
            desc += mParamList.get(i).getConcreteSyntax();

            if (i != mParamList.size() - 1) {
                desc += ",";
            }
        }

        return mName + "( " + desc + " ):" + mClassName + "." + mMethod;
    }

    @Override
    public String getFormattedSyntax() {
        return "";
    }

    @Override
    public FunctionDefinition getCopy() {
        return new FunctionDefinition(mName, mClassName, mMethod, getCopyOfParamList());
    }

    @Override
    public void writeXML(IOSIndentWriter out) {
        out.println("<UserCommand name=\"" + mName + "\" classname =\"" + mClassName + "\" method=\"" + mMethod
                + "\">").push();

        for (int i = 0; i < mParamList.size(); i++) {
            mParamList.get(i).writeXML(out);
        }

        out.pop().println("</UserCommand>");
    }

    @Override
    public void parseXML(Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mMethod = element.getAttribute("method");
        mClassName = element.getAttribute("classname");
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                ArgumentDefinition var = new ArgumentDefinition();

                var.parseXML(element);
                mParamList.add(var);
            }
        });
    }

    public String getParamPrettyPrint() {
        String desc = "(";

        for (int i = 0; i < mParamList.size(); i++) {
            desc += mParamList.get(i).getPrettyType();

            if (i != mParamList.size() - 1) {
                desc += ", ";
            }
        }

        return desc + ")";
    }

    public final boolean isValidClass() {
        boolean isClass = true;
        boolean isObject = true;

        try {
            final Class javaClass = Class.forName(mClassName);
        } catch (ClassNotFoundException ex) {
            isClass = false;
        }

        try {
            int dotIndex = mClassName.lastIndexOf('.');
            String parentName = mClassName.substring(0, dotIndex);
            String memberName = mClassName.substring(dotIndex + 1);
            Class parentClass = Class.forName(parentName);
            Field javaField = parentClass.getField(memberName);
            Class javaClass = javaField.getType();
        } catch (ClassNotFoundException ex) {
            isObject = false;
        } catch (NoSuchFieldException ex) {
            isObject = false;
        } catch (StringIndexOutOfBoundsException ex) {
            isObject = false;
        }

        if (isClass || isObject) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public int compareTo(FunctionDefinition def) {
        return this.getName().compareTo(def.getName());
    }

}
