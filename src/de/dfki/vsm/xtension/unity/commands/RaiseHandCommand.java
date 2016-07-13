package de.dfki.vsm.xtension.unity.commands;

import de.dfki.vsm.runtime.activity.SpeechActivity;

public final class RaiseHandCommand extends Command {
    private final String _actor;
    
    public RaiseHandCommand(int id, String actor) {
        super((byte)0x01, id);
        
        _actor = actor;
    }
    
    public final String getActor() {
        return _actor;
    }
    
    @Override
    public final byte[] GetData() {
        return new byte[] { 0x53, 0x75, 0x63, 0x63, 0x65, 0x73, 0x73 };
    }
    
}