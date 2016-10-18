package de.dfki.vsm.model.sceneflow.definition;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.model.sceneflow.SyntaxObject;
import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.runtime.interpreter.error.InterpreterError;
import de.dfki.vsm.runtime.interpreter.Environment;
import de.dfki.vsm.runtime.interpreter.Evaluator;
import de.dfki.vsm.runtime.interpreter.Interpreter;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public class VarDef implements SyntaxObject {
    private String      mType;
    private String      mName;
    private Expression  mExp;
    private String      mErrorMsg;

    public VarDef() {
        mName = new String();
        mType = new String();
        mExp  = null;
    }

    public VarDef(String name, String type, Expression exp) {
        mName = name;
        mType = type;
        mExp  = exp;
    }

    public void setName(String value) {
        mName = value;
    }

    public String getName() {
        return mName;
    }

    public void setType(String value) {
        mType = value;
    }

    public String getType() {
        return mType;
    }

    public void setExp(Expression value) {
        mExp = value;
    }

    public Expression getExp() {
        return mExp;
    }

    @Override
    public String getAbstractSyntax() {
        return "VarDef(" + mType + "," + mName + "," + ((mExp != null)
                ? mExp.getAbstractSyntax()
                : "") + ")";
    }

    @Override
    public String getConcreteSyntax() {
        return mType + " " + mName + " = " + ((mExp != null)
                ? mExp.getConcreteSyntax()
                : "");
    }

    @Override
    public String getFormattedSyntax() {
        return "#r#" + mType + " " + "" + mName + " = " + ((mExp != null)
                ? mExp.getFormattedSyntax()
                : "");
    }

    @Override
    public VarDef getCopy() {
        return new VarDef(mName, mType, ((mExp != null)
                                         ? mExp.getCopy()
                                         : null));
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<Variable type=\"" + mType + "\" name =\"" + mName + "\">").push();

        if (mExp != null) {
            mExp.writeXML(out);
        }

        out.pop().println("</Variable>");
    }

    @Override
    public void parseXML(Element element) throws XMLParseError {
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
    public String toString() {
        return getConcreteSyntax();
    }
    
    private boolean isMatchingType(String value, String dataType){
        if(dataType.equalsIgnoreCase("BOOL") && value.equalsIgnoreCase("BOOLEAN")){
            return true;
        }
        else if(!value.equalsIgnoreCase(dataType) ){
            return false;
        }
        return true;
    }

    public boolean validate(String name, String type, boolean isNewVar){
        RunTimeProject rt = EditorInstance.getInstance().getSelectedProjectEditor().getEditorProject();
        //rt.
        Interpreter interpreter = new Interpreter(rt);
        Environment env = new Environment();
        env.push();

        Evaluator evaluator = interpreter.getEvaluator();
        for(VarDef var: EditorInstance.getInstance().getSelectedProjectEditor().getEditorProject().getSceneFlow().getVarDefList()){
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
            value = evaluator.evaluate(getExp(),env);
            if(!isMatchingType(value.getType().name(), mType)){
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
        if(isNewVar) {
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
}
