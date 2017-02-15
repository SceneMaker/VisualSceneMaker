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
public final class PlayActionCommand extends Invocation {

    public enum ExecutionMode {
        Default,
        Concurrent,
        Sequential
    };
    private ExecutionMode mMode;
    private Expression mCommand;
    private ArrayList<Expression> mArgList;

    public PlayActionCommand() {
        mMode = ExecutionMode.Concurrent;
        mCommand = null;
        mArgList = new ArrayList();
    }
        
    public PlayActionCommand(
            final Expression arg,
            final ExecutionMode mode) {
        mMode = mode;
        mCommand = arg;
        mArgList = new ArrayList();
    }

    public PlayActionCommand(
            final Expression arg,
            final ArrayList<Expression> argList,
            final ExecutionMode mode) {
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
        String desc = "PlayActionCommand(";
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
        String desc = "PlayActionCommand(";
        desc += ((mCommand != null)
                ? mCommand.getConcreteSyntax()
                : "");
        for (int i = 0; i < mArgList.size(); i++) {
            desc += ", " + mArgList.get(i).getConcreteSyntax();
        }
        return desc + ")";
    }

    // TODO: Check the mode for syntax here
    @Override
    public final String getFormattedSyntax() {
        String desc = "#p#PlayActionCommand ( ";
        desc += ((mCommand != null)
                ? mCommand.getFormattedSyntax()
                : "");
        for (int i = 0; i < mArgList.size(); i++) {
            desc += " , " + mArgList.get(i).getFormattedSyntax();
        }
        return desc + " ) ";
    }

    @Override
    public final PlayActionCommand getCopy() {
        return new PlayActionCommand(mCommand.getCopy(), getCopyOfArgList(), mMode);
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) throws XMLWriteError {
        out.println("<PlayActionCommand mode=\""+ mMode+"\">").push();
        if (mCommand != null) {
            mCommand.writeXML(out);
        }
        for (int i = 0; i < mArgList.size(); i++) {
            mArgList.get(i).writeXML(out);
        }
        out.pop().println("</PlayActionCommand>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        
        mMode = ExecutionMode.valueOf(element.getAttribute("mode"));
        
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
