package de.dfki.vsm.xtension.ssj;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.ssj.event.SSJEventArray;
import de.dfki.vsm.xtension.ssj.event.SSJEventEntry;
import de.dfki.vsm.xtension.ssj.event.data.SSJEventData;
import de.dfki.vsm.xtension.ssj.event.data.SSJTupleData;

import java.util.List;
import java.util.Map;

public class SSJExecutor extends ActivityExecutor implements SSJEventHandler
{
    // The SSI event receiver
    private SSJEventReceiver mReceiver;
    // The SSI event handler
    private SSJEventSender mSender;

    private long startTime;

    public SSJExecutor(PluginConfig config, RunTimeProject project)
    {
        super(config, project);
    }

    @Override
    public String marker(long id)
    {
        return "$(" + id + ")";
    }

    @Override
    public void execute(AbstractActivity activity)
    {
        // set all activities blocking
        activity.setType(AbstractActivity.Type.blocking);

        final String name = activity.getName();

        if ("send".equalsIgnoreCase(name))
        {
            String text = activity.get("text");

            if (text != null)
            {
                // Remove quotes
                if (text.startsWith("'") && text.endsWith("'"))
                {
                    text = text.substring(1, text.length() - 1);
                }

                mSender.sendString(text);
            }
        }
        else if ("event".equalsIgnoreCase(name))
        {
            // Default event values
            String eventName = "output";
            String eventState = "completed";

            List<ActionFeature> features = activity.getFeatures();

            SSJTupleData tupleData = new SSJTupleData();

            for (ActionFeature feature : features)
            {
                String key = feature.getKey();
                String value = feature.getVal();

                // Only if key and value is not empty
                if (key != null && !key.isEmpty() && value != null && !value.isEmpty())
                {
                    if (key.equalsIgnoreCase("name"))
                    {
                        eventName = value;
                    }
                    else if (key.equalsIgnoreCase("state") && (value.equalsIgnoreCase("CONTINUED") || value.equalsIgnoreCase("COMPLETED")))
                    {
                        eventState = value;
                    }
                    else
                    {
                        tupleData.set(key, value);
                    }
                }
            }

            long currentTime = System.currentTimeMillis();

            SSJEventEntry entry = new SSJEventEntry("vsm", eventName, String.valueOf(currentTime - startTime), "1", "1.0", "map", eventState, "-1");
            entry.setData(tupleData);

            SSJEventArray eventArray = new SSJEventArray("1.0");
            eventArray.add(entry);

            String xml = eventArray.toString();

            if (xml != null)
            {
                mSender.sendString(xml);
            }
        }
    }

    @Override
    public void launch()
    {
        mLogger.message("Launching SSJExecutor");

        // Get the plugin configuration
        final String rlhost = mConfig.getProperty("vsm_rec_host"); // Receiver Local Host (VSM receiver host)
        final String rlport = mConfig.getProperty("vsm_rec_port"); // Receiver Local Port (VSM receiver port)
        final String slhost = mConfig.getProperty("vsm_send_host"); // Sender Local Host (VSM sender host)
        final String slport = mConfig.getProperty("vsm_send_port"); // Sender Local Port (VSM sender port)
        final String srhost = mConfig.getProperty("ssj_rec_host"); // Sender Remote Host (SSJ receiver host)
        final String srport = mConfig.getProperty("ssj_rec_port"); // Sender Remote Port (SSJ receiver port)

        // Initialize the event receiver
        mReceiver = new SSJEventReceiver(this, rlhost, Integer.parseInt(rlport));
        // Initialize the event sender
        mSender = new SSJEventSender(slhost, Integer.parseInt(slport), srhost, Integer.parseInt(srport));
        // Start the SSI event receiver
        mReceiver.start();
        // Start the SSI event sender
        mSender.start();

        startTime = System.currentTimeMillis();
    }

    @Override
    public void unload()
    {
        mLogger.message("Unloading SSJExecutor");

        // Abort the SSI event receiver
        mReceiver.abort();
        // Abort the SSI event sender
        mSender.abort();
        // Join the SSI event threads
        try
        {
            // Join the SSI event receiver
            mReceiver.join();
            // Join the SSI event sender
            mSender.join();
        }
        catch (final InterruptedException exc)
        {
            mLogger.failure(exc.toString());
        }
    }

    @Override
    public void handle(SSJEventArray array)
    {
        if (mProject.isRunning())
        {
            // Print some information
            for (final SSJEventEntry event : array.list())
            {
                final SSJEventData data = event.getData();

                // mLogger.message("Received SSJ event: " + data);

                if ("map".equalsIgnoreCase(event.getType()) || "ntuple".equalsIgnoreCase(event.getType()))
                {
                    Map<String, String> tupleData = ((SSJTupleData) data).getTupleMap();

                    for (String key : tupleData.keySet())
                    {
                        String value = tupleData.get(key);
                        String shortKey = key.substring(2);

                        if (key.startsWith("i_"))
                        {
                            mProject.setVariable(shortKey, Integer.parseInt(value));
                        }
                        else if (key.startsWith("b_"))
                        {
                            mProject.setVariable(shortKey, Boolean.parseBoolean(value));
                        }
                        else if (key.startsWith("f_"))
                        {
                            mProject.setVariable(shortKey, Float.parseFloat(value));
                        }
                        else if (key.startsWith("s_"))
                        {
                            mProject.setVariable(shortKey, value);
                        }
                        else
                        {
                            mProject.setVariable(key, value);
                        }
                    }
                }
                else
                {
                    String key = event.getSender() + "_" + event.getEvent();

                    mProject.setVariable(key, data.toString().trim());
                }
            }
        }
    }
}
