import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.*;

import java.util.Random;

/*
 * Numbers are added to the belt by a producer, a producer is a factory worker
 */
public class Producer extends FactoryWorker {

    public Producer (int identification, NumberQueue q) {
        super("Producer", identification, q);
    }

    @Override
    public void message(int msg) {
        System.out.println("Producer " + id + " produced " + msg);
    }

    @Override
    public void run() {

        while (!Thread.currentThread().isInterrupted()) {
            try {
                message(action());
            } catch (IndexOutOfBoundsException e) {
                messageError();
            }
        }
    }

    @Override
    public int action() {
        Random r = new Random();
        int number = r.nextInt(10000);

        belt.enqueue(number);
        return number;
    }
}
