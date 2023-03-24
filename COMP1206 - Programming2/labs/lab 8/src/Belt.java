/*
 * Numbers are stored by a number factory by using this belt Class
 */
public class Belt extends CyclicQueue {

    private Object eLock = new Object(); /* lock that adds a number to the belt */
    private Object dLock = new Object(); /* lock that removes a number from the belt */

    public Belt(int s) {
        super(s);
    }

    @Override
    public void enqueue(int n) throws IndexOutOfBoundsException {
        synchronized (eLock) {

            while (s == cQueue.length) {
                try {
                    synchronized (this) {
                        this.wait();
                    }
                } catch (InterruptedException e) {}
            }

            super.enqueue(n);
            synchronized (this) {
                this.notify();
            }
        }
    }

    @Override
    public int dequeue() throws IndexOutOfBoundsException {
        synchronized (dLock) {

            while (s == 0) {
                try {
                    synchronized (this) {
                        this.wait();
                    }
                } catch (InterruptedException e) {}
            }

            int number = super.dequeue();
            synchronized (this) {
                this.notify();
            }
            return number;
        }
    }
}
