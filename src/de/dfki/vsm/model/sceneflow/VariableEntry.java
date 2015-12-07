package de.dfki.vsm.model.sceneflow;


import java.text.AttributedString;

public class VariableEntry {
        private SuperNode        mSuperNode;
        private boolean          mHasChanged;
        private String           mConcrete;
        private String           mFormatted;
        private AttributedString mAttributed;

        public VariableEntry(SuperNode superNode, boolean hasChanged, String concrete, String formatted,
                     AttributedString attributed) {
            mSuperNode  = superNode;
            mHasChanged = hasChanged;
            mConcrete   = concrete;
            mFormatted  = formatted;
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
        public String getVarValue()
        {
            String[] s = mFormatted.split(" ");
            if (s.length > 3) {
                return s[3];
            } else {
                return "";
            }
        }

        public SuperNode getSuperNode() {
            return mSuperNode;
        }

        public void setSuperNode(SuperNode mSuperNode) {
            this.mSuperNode = mSuperNode;
        }

        public boolean isHasChanged() {
            return mHasChanged;
        }

        public void setHasChanged(boolean mHasChanged) {
            this.mHasChanged = mHasChanged;
        }

        public String getConcrete() {
            return mConcrete;
        }

        public void setConcrete(String mConcrete) {
            this.mConcrete = mConcrete;
        }

        public String getFormatted() {
            return mFormatted;
        }

        public void setFormatted(String mFormatted) {
            this.mFormatted = mFormatted;
        }

        public AttributedString getAttributed() {
            return mAttributed;
        }

        public void setAttributed(AttributedString mAttributed) {
            this.mAttributed = mAttributed;
        }

    }