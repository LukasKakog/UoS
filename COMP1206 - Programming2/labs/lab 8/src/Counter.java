import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.*;

/*
 * keeps a count of the number of guests
 */
public class Counter implements UnitCounter{
    private int c;

    public synchronized void addOne() {
        c++;
    }

    public synchronized int getCounter() {
        return this.c;
    }
}
