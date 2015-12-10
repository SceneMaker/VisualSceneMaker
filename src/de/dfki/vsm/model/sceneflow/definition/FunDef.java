package de.dfki.vsm.model.sceneflow.definition;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.Syntax;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

//~--- JDK imports ------------------------------------------------------------

import java.lang.reflect.Field;

import java.util.Vector;

/**
 * A user command definition.
 *
 * @author Not me
 */
public class FunDef extends Syntax implements Comparable<FunDef> {
    private String           mName;
    private String           mClassName;
    private String           mMethod;
    private Vector<ParamDef> mParamList;
    private boolean          active = true;

    
    public FunDef() {
        mName      = new String();
        mClassName = new String();
        mMethod    = new String();
        mParamList = new Vector<ParamDef>();
    }

    public FunDef(String name, String className, String method) {
        mName      = name;
        mClassName = className;
        mMethod    = method;
        mParamList = new Vector<ParamDef>();
    }

    public FunDef(String name, String className, String method, Vector<ParamDef> paramList) {
        mName      = name;
        mClassName = className;
        mMethod    = method;
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

    public Vector<ParamDef> getParamList() {
        return mParamList;
    }

    public void setParamList(Vector<ParamDef> value) {
        mParamList = value;
    }

    public int getSizeOfParamList() {
        return mParamList.size();
    }

    public Vector<ParamDef> getCopyOfParamList() {
        Vector<ParamDef> copy = new Vector<ParamDef>();

        for (ParamDef param : mParamList) {
            copy.add(param.getCopy());
        }

        return copy;
    }

    public boolean addParam(ParamDef value) {
        return mParamList.add(value);
    }

    public ParamDef getParamAt(int index) {
        return mParamList.get(index);
    }

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

    public String getFormattedSyntax() {
        return "";
    }

    public FunDef getCopy() {
        return new FunDef(mName, mClassName, mMethod, getCopyOfParamList());
    }

    public void writeXML(IOSIndentWriter out) {
        out.println("<UserCommand name=\"" + mName + "\" classname =\"" + mClassName + "\" method=\"" + mMethod
                    + "\">").push();

        for (int i = 0; i < mParamList.size(); i++) {
            mParamList.get(i).writeXML(out);
        }

        out.pop().println("</UserCommand>");
    }

    public void parseXML(Element element) throws XMLParseError {
        mName      = element.getAttribute("name");
        mMethod    = element.getAttribute("method");
        mClassName = element.getAttribute("classname");
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                ParamDef var = new ParamDef();

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
        boolean isClass  = true;
        boolean isObject = true;

        try {
            final Class javaClass = Class.forName(mClassName);
        } catch (ClassNotFoundException ex) {
            isClass = false;
        }

        try {
            int    dotIndex    = mClassName.lastIndexOf('.');
            String parentName  = mClassName.substring(0, dotIndex);
            String memberName  = mClassName.substring(dotIndex + 1);
            Class  parentClass = Class.forName(parentName);
            Field  javaField   = parentClass.getField(memberName);
            Class  javaClass   = javaField.getType();
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
    public int compareTo(FunDef def) {
        return this.getName().compareTo(def.getName());
    }
}
