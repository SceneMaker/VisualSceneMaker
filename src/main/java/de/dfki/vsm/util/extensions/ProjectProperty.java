package de.dfki.vsm.util.extensions;

/**
 * Created by alvaro on 4/20/17.
 */
public class ProjectProperty {


    private final String description;
    String name;
    boolean required;

    public ProjectProperty(String name, boolean required){
        this.name = name;
        this.required = required;
        this.description = "";
    }

    public ProjectProperty(String name ){
        this.name = name;
        this.required = false;
        this.description = "";
    }

    public ProjectProperty(String name, String description){
        this.name = name;
        this.description = description;
        this.required = false;
    }

    public ProjectProperty(String name, boolean required, String description){
        this.name = name;
        this.description = description;
        this.required = required;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public boolean isRequired() {
        return required;
    }

    public void setRequired(boolean required) {
        this.required = required;
    }

    public String getDescription(){
        return description;
    }

    public String toString(){
        String requireText = "";
        if(required)
            requireText = " (required)";
        return this.getName() + requireText;
    }
}
