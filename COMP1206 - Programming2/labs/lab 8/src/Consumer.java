import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.FactoryWorker;
import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.NumberQueue;

/*
 * Numbers are taken from the factory belt by the consumer, a consumer is a factory worker
 */
public class Consumer extends FactoryWorker {

    public Consumer (int identification, NumberQueue q) {
        super("Consumer", identification, q);
    }

    @Override
    public void message(int msg) {
        System.out.println("Consumer " + id + " picked " + msg);
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
        return belt.dequeue();
    }

}
