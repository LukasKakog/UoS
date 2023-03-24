import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.*;

/*
 * Integers are stored in this cyclic queue
 */
public class CyclicQueue implements NumberQueue {
    protected int[] cQueue;
    protected int h;
    protected int t;
    protected int s;

    public CyclicQueue(int s) {
        cQueue = new int[s];
        h = 0;
        t = -1;
        this.s = 0;
    }

    @Override
    public void enqueue(int i) throws IndexOutOfBoundsException {

        if (s == cQueue.length) { throw new IndexOutOfBoundsException("Full Queue"); }

        if (t == cQueue.length - 1) { t = 0; }
        else t++;

        cQueue[t] = i;
        s++;
    }

    @Override
    public int dequeue() throws IndexOutOfBoundsException {
        if (s == 0) { throw new IndexOutOfBoundsException("Empty Queue"); }

        int component = cQueue[h];

        if (h == cQueue.length - 1) { h = 0; }
        else h++;

        s--;
        return component;
    }

    @Override
    public boolean isEmpty() {
        return s == 0;
    }
}
