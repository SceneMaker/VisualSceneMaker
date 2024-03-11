package de.dfki.vsm.xtension.DriveSimulator.gson;

public class Vehicle {
    int id;
    double distance;
    double distToRef;
    double roadS;
    int laneId;
    double speed;
    double length;
    double width;
    double height;

    public Vehicle(int id, double distance, double distToRef, double roadS, int laneId, double speed, double length, double width, double height) {
        this.id = id;
        this.distance = distance;
        this.distToRef = distToRef;
        this.roadS = roadS;
        this.laneId = laneId;
        this.speed = speed;
        this.length = length;
        this.width = width;
        this.height = height;
    }

    public Vehicle() {
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public double getDistance() {
        return distance;
    }

    public void setDistance(double distance) {
        this.distance = distance;
    }

    public double getDistToRef() {
        return distToRef;
    }

    public void setDistToRef(double distToRef) {
        this.distToRef = distToRef;
    }

    public double getRoadS() {
        return roadS;
    }

    public void setRoadS(double roadS) {
        this.roadS = roadS;
    }

    public int getLaneId() {
        return laneId;
    }

    public void setLaneId(int laneId) {
        this.laneId = laneId;
    }

    public double getSpeed() {
        return speed;
    }

    public void setSpeed(double speed) {
        this.speed = speed;
    }

    public double getLength() {
        return length;
    }

    public void setLength(double length) {
        this.length = length;
    }

    public double getWidth() {
        return width;
    }

    public void setWidth(double width) {
        this.width = width;
    }

    public double getHeight() {
        return height;
    }

    public void setHeight(double height) {
        this.height = height;
    }
}
