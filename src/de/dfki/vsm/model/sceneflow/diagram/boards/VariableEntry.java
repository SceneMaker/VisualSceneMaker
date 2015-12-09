package de.dfki.vsm.model.sceneflow.diagram.boards;

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.model.sceneflow.diagram.nodes.SuperNode;
import java.text.AttributedString;

public class VariableEntry /*implements*/ /*SyntaxObject*/ /*ModelObject */{

    public SuperNode mSuperNode;
    public boolean mHasChanged;
    public String mConcrete;
    public String mFormatted;
    public AttributedString mAttributed;

    public VariableEntry(
            SuperNode superNode, 
            boolean hasChanged, 
            String concrete, 
            String formatted,
            AttributedString attributed) {
        mSuperNode = superNode;
        mHasChanged = hasChanged;
        mConcrete = concrete;
        mFormatted = formatted;
        mAttributed = attributed;
    }

    public String getVarName() {
        String[] s = mConcrete.split(" ");

        if (s.length > 2) {
            return s[1];
        } else {
            return "";
        }
    }

    public String getVarType() {
        String[] s = mConcrete.split(" ");

        if (s.length > 2) {
            return s[0];
        } else {
            return "";
        }
    }

    public String getVarValue() {
        String[] s = mFormatted.split(" ");
        if (s.length > 3) {
            return s[3];
        } else {
            return "";
        }
    }
    
    /*
     @Override
    public VariableEntry getCopy() {
        return null;
    }
    */
}
