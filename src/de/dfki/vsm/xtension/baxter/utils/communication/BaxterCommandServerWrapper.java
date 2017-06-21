package de.dfki.vsm.xtension.baxter.utils.communication;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLUtilities;
import de.dfki.vsm.xtension.baxter.BaxterHandler;
import de.dfki.vsm.xtension.baxter.command.BaxterCommand;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;

/**
 * Created by alvaro on 8/27/16.
 */
public class BaxterCommandServerWrapper {
    protected  BaxterHandler handler;
    private int commandCounter = 0;

    public BaxterCommandServerWrapper(BaxterHandler pHandler){
        handler = pHandler;
    }


    public  BaxterCommand BaxterBuildCommand(final String commandName, final ArrayList<String> params) {
        isHandlerNull();
        //TODO: Degree information
        String id = "ID_"+ commandName + "_" + commandCounter;
        final BaxterCommand command = new BaxterCommand(commandName, "ID", params);
        return  command;
    }

    protected  void isHandlerNull() {
        if(null == handler){
            throw new ExceptionInInitializerError("No handler specified");
        }
    }

    public void sendToServer(BaxterCommand command){
        String toSend = getCommandAsString(command);
        handler.send(toSend);
    }

    protected String getCommandAsString(BaxterCommand command) {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        IOSIndentWriter iosw = new IOSIndentWriter(out);
        boolean r = XMLUtilities.writeToXMLWriter(command, iosw);
        String toSend = new String(out.toByteArray());
        toSend += "#END\n";
        return toSend;
    }
}
