package de.dfki.vsm.xtension.DriveSimulator.gson;

public class EgoVehicle extends Vehicle {
    private int apStatus;
    private int indicator;
    private int frontVehicleId;
    private double desiredSpeed;
    private double speedLimit;
    private double brake;
    private double throttle;
    private double laneOffset;

    public EgoVehicle(int id, double distance, double distToRef, double roadS, int laneId, double speed, double length, double width, double height, int apStatus, int indicator, int frontVehicleId, double desiredSpeed, double speedLimit, double brake, double throttle, double laneOffset) {
        super();
        this.id = id;
        this.distance = distance;
        this.distToRef = distToRef;
        this.roadS = roadS;
        this.laneId = laneId;
        this.speed = speed;
        this.length = length;
        this.width = width;
        this.height = height;
        this.apStatus = apStatus;
        this.indicator = indicator;
        this.frontVehicleId = frontVehicleId;
        this.desiredSpeed = desiredSpeed;
        this.speedLimit = speedLimit;
        this.brake = brake;
        this.throttle = throttle;
        this.laneOffset = laneOffset;
    }

    public int getApStatus() {
        return apStatus;
    }

    public void setApStatus(int apStatus) {
        this.apStatus = apStatus;
    }

    public int getIndicator() {
        return indicator;
    }

    public void setIndicator(int indicator) {
        this.indicator = indicator;
    }

    public int getFrontVehicleId() {
        return frontVehicleId;
    }

    public void setFrontVehicleId(int frontVehicleId) {
        this.frontVehicleId = frontVehicleId;
    }

    public double getDesiredSpeed() {
        return desiredSpeed;
    }

    public void setDesiredSpeed(double desiredSpeed) {
        this.desiredSpeed = desiredSpeed;
    }

    public double getSpeedLimit() {
        return speedLimit;
    }

    public void setSpeedLimit(double speedLimit) {
        this.speedLimit = speedLimit;
    }

    public double getBrake() {
        return brake;
    }

    public void setBrake(double brake) {
        this.brake = brake;
    }

    public double getThrottle() {
        return throttle;
    }

    public void setThrottle(double throttle) {
        this.throttle = throttle;
    }

    public double getLaneOffset() {
        return laneOffset;
    }

    public void setLaneOffset(double laneOffset) {
        this.laneOffset = laneOffset;
    }
}
