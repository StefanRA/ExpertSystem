/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package expertsystemgui;

import java.awt.image.BufferedImage;
import org.joda.time.LocalDate;

/**
 *
 * @author Claudiu
 */
public class Solution implements Comparable<Solution>{
    
    private String goal;
    private String value;
    private int certaintyFactor;
    private String name;
    private String description;
    private String domain;
    private BufferedImage image;
    private LocalDate date;
    private String location;
    
    public Solution(){
    }

    public Solution(String goal,
                    String value,
                    int certaintyFactor, 
                    String name,
                    String description, 
                    String domain, 
                    BufferedImage image, 
                    LocalDate date,
                    String location) {
        this.goal = goal;
        this.value = value;
        this.certaintyFactor = certaintyFactor;
        this.name = name;
        this.description = description;
        this.image = image;
        this.domain = domain;
        this.date = date;
        this.location = location;
    }
    
    public String getGoal() {
        return goal;
    }

    public void setGoal(String goal) {
        this.goal = goal;
    }
    
    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public int getCertaintyFactor() {
        return certaintyFactor;
    }

    public void setCertaintyFactor(int certaintyFactor) {
        this.certaintyFactor = certaintyFactor;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
    
    
    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }
    
    public BufferedImage getImage() {
        return image;
    }

    public void setImage(BufferedImage image) {
        this.image = image;
    }

    public String getDomain() {
        return domain;
    }

    public void setDomain(String domain) {
        this.domain = domain;
    }

    public LocalDate getDate() {
        return date;
    }

    public void setDate(LocalDate date) {
        this.date = date;
    }

    public String getLocation() {
        return location;
    }

    public void setLocation(String location) {
        this.location = location;
    }
    
    @Override
    public String toString() {
        return goal + " este " + value + " cu fc " + certaintyFactor;
    }

    @Override
    public int compareTo(Solution other) {
        //ascending
        //return Integer.compare(this.certaintyFactor, other.certaintyFactor);
        
        //descending
        return Integer.compare(other.certaintyFactor, this.certaintyFactor);
    }
    
}
