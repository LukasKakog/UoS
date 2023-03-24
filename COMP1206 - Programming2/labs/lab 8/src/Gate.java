/*
 * Gate allows guests to enter the factory and counter is updated
 */
public class Gate implements Runnable {
    int nOfGuests;
    Counter c;

    public Gate(Counter c, int nOfGuests) {
        this.c = c;
        this.nOfGuests = nOfGuests;
    }

    @Override
    public void run() {
        for (int i = 0; i < nOfGuests; i++) {
            c.addOne();
        }
    }
}
