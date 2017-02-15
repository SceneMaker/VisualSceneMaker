package de.dfki.vsm.model.sceneflow.glue.command.definition;

import de.dfki.vsm.model.sceneflow.glue.command.Definition;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class VariableDefinition extends Definition {

    private String mType;
    private String mName;
    private Expression mExp;

    public VariableDefinition() {
        mName = null;
        mType = null;
        mExp = null;
    }

    public VariableDefinition(final String name, final String type, final Expression exp) {
        mName = name;
        mType = type;
        mExp = exp;
    }

    public final void setName(final String value) {
        mName = value;
    }

    public final String getName() {
        return mName;
    }

    public final void setType(final String value) {
        mType = value;
    }

    public final String getType() {
        return mType;
    }

    public final void setExp(final Expression value) {
        mExp = value;
    }

    public final Expression getExp() {
        return mExp;
    }

    @Override
    public final String getAbstractSyntax() {
        return "VariableDefinition(" + mType + "," + mName + "," + ((mExp != null)
                ? mExp.getAbstractSyntax()
                : "") + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return mType + " " + mName + " = " + ((mExp != null)
                ? mExp.getConcreteSyntax()
                : "");
    }

    @Override
    public final String getFormattedSyntax() {
        return "#r#" + mType + " " + "" + mName + " = " + ((mExp != null)
                ? mExp.getFormattedSyntax()
                : "");
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) throws XMLWriteError {
        out.println("<VariableDefinition type=\"" + mType + "\" name =\"" + mName + "\">").push();
        if (mExp != null) {
            mExp.writeXML(out);
        }
        out.pop().println("</VariableDefinition>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mType = element.getAttribute("type");
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(Element element) throws XMLParseError {
                mExp = Expression.parse(element);
            }
        });
    }

    @Override
    public final VariableDefinition getCopy() {
        return new VariableDefinition(mName, mType, ((mExp != null)
                ? mExp.getCopy()
                : null));
    }

    /*
    private boolean isMatchingType(String value, String dataType) {
        if (dataType.equalsIgnoreCase("BOOL") && value.equalsIgnoreCase("BOOLEAN")) {
            return true;
        } else if (!value.equalsIgnoreCase(dataType)) {
            return false;
        }
        return true;
    }

    
    public boolean validate(String name, String type, boolean isNewVar) {
        RunTimeProject rt = EditorInstance.getInstance().getSelectedProjectEditor().getEditorProject();
        //rt.
        Interpreter interpreter = new Interpreter(rt);
        Environment env = new Environment();
        env.push();

        Evaluator evaluator = interpreter.getEvaluator();
        for (VariableDefinition var : EditorInstance.getInstance().getSelectedProjectEditor().getEditorProject().getSceneFlow().getVarDefList()) {
            try {
                evaluator.declare(var, env);
            } catch (InterpreterError e) {
                e.printStackTrace();
                setmErrorMsg("Do not match the data type selected");
                return false;
            }

        }
        //Try to evaluate:
        AbstractValue value;
        try {
            value = evaluator.evaluate(getExp(), env);
            if (!isMatchingType(value.getType().name(), mType)) {
                setmErrorMsg("Do not match the data type selected");
                return false;
            }

        } catch (InterpreterError e) {
            e.printStackTrace();
            setmErrorMsg(e.getMessage());
            return false;

        }

        mName = name;
        mType = type;
        if (isNewVar) {
            try {
                evaluator.declare(this, env);
            } catch (InterpreterError e) {
                e.printStackTrace();
                setmErrorMsg(e.getMessage());
                return false;
            }
        }

        return true;
    }

    public String getmErrorMsg() {
        return mErrorMsg;
    }

    private void setmErrorMsg(String mErrorMsg) {
        this.mErrorMsg = mErrorMsg;
    }
     */
}
