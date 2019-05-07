/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.button;

/**
 *
 * @author Patrick Gebhard
 * 
 */
public class ButtonValues {

    String mId;
    int mX;
    int mY;
    int mSize;
    String mName;
    String mValue;
    String mVSMVar;

    public ButtonValues(String id, int x, int y, int size, String name, String value, String vsmvar) {
        mId = id;
        mX = x;
        mY = y;
        mSize = size;
        mName = name;
        mValue = value;
        mVSMVar = vsmvar;
    }
}
