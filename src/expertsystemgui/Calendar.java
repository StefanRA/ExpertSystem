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
public class Calendar
{
    public int rows;
    public int columns = 13;
    public String[] columnNames;
    public Object[][] data;

    public Calendar(int rows, int columns, String[] columnNames, Object[][] data) {
        this.rows = rows;
        this.columns = columns;
        this.columnNames = columnNames;
        this.data = data;
    }
}
