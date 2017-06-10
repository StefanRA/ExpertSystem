/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package expertsystemgui;

/**
 *
 * @author Claudiu
 */
public class Solution implements Comparable<Solution>{
    
    private String goal;
    private String value;
    private int certaintyFactor;

    public Solution(){
    }
    
    public Solution(String goal, String value, int certaintyFactor) {
        this.goal = goal;
        this.value = value;
        this.certaintyFactor = certaintyFactor;
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

    @Override
    public String toString() {
        return goal + " este " + value + " cu fc " + certaintyFactor;
    }

    
    /**
     * Transforms a string into a Solution object.
     * @param s should have the following format "goal este value cu fc certaintyFactor"
     * @return Solution object with the given attributes
     */
    public static Solution toSolution(String s){
        String[] words = s.split(" ");
        if(words.length < 6 || !words[1].equals("este"))
            return null;
        
        return new Solution(words[0], words[2], Integer.parseInt(words[5]));
    }

    @Override
    public int compareTo(Solution other) {
        //ascending
        //return Integer.compare(this.certaintyFactor, other.certaintyFactor);
        
        //descending
        return Integer.compare(other.certaintyFactor, this.certaintyFactor);
    }
    
}
