package de.dfki.vsm.util;

import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.project.RunTimeProject;

import java.io.FileWriter;
import java.io.IOException;
import java.util.Date;
import java.util.LinkedList;
import java.util.stream.Stream;

public class ActivityLogger {

    private final RunTimeProject _prj ;
    private final FileWriter _fw ;

    private final static String[] header_names = new String[] {"timestamp", "date", "actor","text","name","type","features","variables"} ;
    private final static String header = String.join("\t", header_names) ;

    public ActivityLogger(String log_prefix, RunTimeProject prj) throws IOException {
        _prj = prj ;

        long timestamp = System.currentTimeMillis() ;
        Date now = new Date(timestamp) ;
        String log_filename = log_prefix + "-" + "activity_log" + now.toString() + ".csv" ;
        _fw = new FileWriter(log_filename) ;
        _fw.write(header);
        _fw.write("\n");
        _fw.flush();
    }

    public void close() throws IOException {
        _fw.flush();
        _fw.close();
    }

    public void log(AbstractActivity activity, String[] variable_names) throws IOException {

        String actor = "";
        String text = "";
        String name = "";
        String type = "";
        String features = "" ;

        if (activity != null) {
            actor = activity.getActor() ;
            text = activity.getText() ;
            name = activity.getName() ;
            type = activity.getType().toString() ;
            if(activity.getFeatures() != null) {
                String[] ff = activity.getFeatures().stream().map(f -> f.getKey() + "=" + f.getValNoQuotes()).toArray(String[]::new);
                features = String.join(",", ff) ;
            }
        }

        String variables = "";
        LinkedList<String> variable_values ;
        if (variable_names != null) {
            variable_values = new LinkedList<>() ;
            for (String var: variable_names) {
                if(_prj.hasVariable(var)) {
                    String val = _prj.getValueOf(var).toString() ;
                    variable_values.add(var + "=" + val) ;
                }
            }
            variables = String.join(",", variable_values) ;
        }


        long timestamp = System.currentTimeMillis() ;

        String line = String.join("\t",
                timestamp + "",
                (new Date(timestamp)).toString(),
                actor,
                text,
                name,
                type,
                features,
                variables) ;

        _fw.write(line + "\n");
        _fw.flush();
    }

    public void logActivity(AbstractActivity activity) throws IOException {
        log(activity, null);
    }

    public void logVariables(String[] variable_names) throws IOException {
        log(null, variable_names) ;
    }

}
