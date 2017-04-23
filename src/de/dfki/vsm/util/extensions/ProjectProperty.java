package de.dfki.vsm.util.extensions;

/**
 * Created by alvaro on 4/20/17.
 */
public class ProjectProperty {


    String name;
    boolean required;

    public ProjectProperty(String name, boolean required){
        this.name = name;
        this.required = required;
    }

    public ProjectProperty(String name ){
        this.name = name;
        this.required = false;
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

    public String toString(){
        return this.getName();
    }
}
