package de.dfki.vsm.xtension.DriveSimulator.gson;

public class ConstructionDistance {
    private double constructionStart;
    private double constructionEnd;
    private double fourLanes;
    private double exit;

    public ConstructionDistance(double constructionStart, double constructionEnd, double fourLanes, double exit) {
        this.constructionStart = constructionStart;
        this.constructionEnd = constructionEnd;
        this.fourLanes = fourLanes;
        this.exit = exit;
    }

    // Getter methods
    public double getConstructionStart() {
        return constructionStart;
    }

    public double getConstructionEnd() {
        return constructionEnd;
    }

    public double getFourLanes() {
        return fourLanes;
    }

    public double getExit() {
        return exit;
    }

    public void setConstructionStart(double constructionStart) {
        this.constructionStart = constructionStart;
    }

    public void setConstructionEnd(double constructionEnd) {
        this.constructionEnd = constructionEnd;
    }

    public void setFourLanes(double fourLanes) {
        this.fourLanes = fourLanes;
    }

    public void setExit(double exit) {
        this.exit = exit;
    }

    @Override
    public String toString() {
        return "ConstructionDetails{" +
                "constructionStart=" + constructionStart +
                ", constructionEnd=" + constructionEnd +
                ", fourLanes=" + fourLanes +
                ", exit=" + exit +
                '}';
    }
}