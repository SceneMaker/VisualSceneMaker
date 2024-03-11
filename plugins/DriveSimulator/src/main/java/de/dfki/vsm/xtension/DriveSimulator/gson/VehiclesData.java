package de.dfki.vsm.xtension.DriveSimulator.gson;

import java.util.List;

public class VehiclesData {
    private List<Vehicle> vehicles;
    private int vehicleCount;

    public List<Vehicle> getVehicles() {
        return vehicles;
    }

    public Vehicle getEgoVehicle(){
        for(Vehicle v:vehicles){
            if (v.getId() == 0) return v;
        }
        return null;
    }

    public void setVehicles(List<Vehicle> vehicles) {
        this.vehicles = vehicles;
    }

    public int getVehicleCount() {
        return vehicleCount;
    }

    public void setVehicleCount(int vehicleCount) {
        this.vehicleCount = vehicleCount;
    }
}

