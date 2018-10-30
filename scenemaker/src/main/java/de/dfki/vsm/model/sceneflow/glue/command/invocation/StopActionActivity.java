package de.dfki.vsm.model.sceneflow.glue.command.invocation;

import de.dfki.vsm.model.sceneflow.glue.command.Invocation;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import java.util.ArrayList;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class StopActionActivity extends Invocation {

    public enum StopMode {
        Default,
        Operator
    };
    private StopMode mMode;
    private Expression mCommand;
    private ArrayList<Expression> mArgList;

    public StopActionActivity() {
        mMode = StopMode.Default;
        mCommand = null;
        mArgList = new ArrayList();
    }

    public StopActionActivity(
            final Expression arg,
            final StopMode mode) {
        mMode = mode;
        mCommand = arg;
        mArgList = new ArrayList();
    }

    public StopActionActivity(
            final Expression arg,
            final ArrayList<Expression> argList,
            final StopMode mode) {
        mMode = mode;
        mCommand = arg;
        mArgList = argList;
    }

    public final Expression getCommand() {
        return mCommand;
    }

    public final void setCommand(final Expression arg) {
        mCommand = arg;
    }

    public final ArrayList<Expression> getArgList() {
        return mArgList;
    }

    public void setArgList(ArrayList<Expression> value) {
        mArgList = value;
    }

    public final ArrayList<Expression> getCopyOfArgList() {
        final ArrayList<Expression> copy = new ArrayList();
        for (final Expression exp : mArgList) {
            copy.add(exp.getCopy());
        }
        return copy;
    }

    @Override
    public final String getAbstractSyntax() {
        String desc = "StopAction(";
        desc += ((mCommand != null)
                ? mCommand.getAbstractSyntax()
                : "");
        for (int i = 0; i < mArgList.size(); i++) {
            desc += ", " + mArgList.get(i).getAbstractSyntax();
        }
        return desc + ")";
    }

    @Override
    public final String getConcreteSyntax() {

        String desc = (mMode == StopMode.Default ? "StopAction(" : "!~");
        desc += ((mCommand != null)
                ? mCommand.getConcreteSyntax()
                : "");
        for (int i = 0; i < mArgList.size(); i++) {
            desc += ", " + mArgList.get(i).getConcreteSyntax();
        }
        return desc + (mMode == StopMode.Default ? ")" : ".");
    }

    // TODO: Check the mode for syntax here
    @Override
    public final String getFormattedSyntax() {
        String desc = "#p#" + (mMode == StopMode.Default ? "StopAction ( " : "!~ ");
        desc += ((mCommand != null)
                ? mCommand.getFormattedSyntax()
                : "");
        for (int i = 0; i < mArgList.size(); i++) {
            desc += " , " + mArgList.get(i).getFormattedSyntax();
        }
        return desc + (mMode == StopMode.Default ? " ) " : " .");
    }

    @Override
    public final StopActionActivity getCopy() {
        return new StopActionActivity(mCommand.getCopy(), getCopyOfArgList(), mMode);
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) throws XMLWriteError {
        out.println("<StopAction mode=\"" + mMode + "\">").push();
        if (mCommand != null) {
            mCommand.writeXML(out);
        }
        for (int i = 0; i < mArgList.size(); i++) {
            mArgList.get(i).writeXML(out);
        }
        out.pop().println("</StopAction>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {

        mMode = StopMode.valueOf(element.getAttribute("mode"));

        final ArrayList<Expression> expList = new ArrayList();
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(Element element) throws XMLParseError {
                expList.add(Expression.parse(element));
            }
        });
        mCommand = expList.remove(0);
        mArgList = expList;
    }
}
