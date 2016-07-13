package de.dfki.vsm.xtension.unity.commands;

import de.dfki.vsm.runtime.activity.SpeechActivity;

public final class SpeechCommand extends Command {
    private final String _actor;
    private final String _text;
    
    public SpeechCommand(int id, SpeechActivity activity) {
        super((byte)0x01, id);
                
        _actor = activity.getActor();
        _text = activity.toString() + activity.getPunctuation();
    }
    
    public SpeechCommand(int id, String actor, String text) {
        super((byte)0x01, id);
        
        _actor = actor;
        _text = text;
    }
    
    public final String getActor() {
        return _actor;
    }
    
    public final String getText() {
        return _text;
    }
   
    @Override
    public final byte[] GetData() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
    
}