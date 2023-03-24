import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.Seat;
import java.util.concurrent.locks.ReentrantLock;

/*
 * This class creates 1 instance of this type of seat
 */
public class Type2 implements Seat {
    private ReentrantLock fLock1;
    private ReentrantLock fLock2;

    @Override
    public void assignForks(ReentrantLock rLock, ReentrantLock rLock1) {
        fLock1 = rLock;
        fLock2 = rLock1;
    }

    @Override
    public void askFork1() {
        fLock1.lock();
        askFork2();
    }

    @Override
    public void askFork2() {
        fLock2.lock();
    }
}
