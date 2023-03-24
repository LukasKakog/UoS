package uk.ac.soton.comp1206.component;

import javafx.animation.AnimationTimer;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.value.ObservableValue;
import javafx.scene.canvas.Canvas;
import javafx.scene.paint.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * The Visual User Interface component representing a single block in the grid.
 *
 * Extends Canvas and is responsible for drawing itself.
 *
 * Displays an empty square (when the value is 0) or a coloured square depending on value.
 *
 * The GameBlock value should be bound to a corresponding block in the Grid model.
 */
public class GameBlock extends Canvas {

    private static final Logger logger = LogManager.getLogger(GameBlock.class);

    /**
     * The set of colours for different pieces
     */
    public static final Color[] COLOURS = {
            Color.TRANSPARENT,
            Color.DEEPPINK,
            Color.RED,
            Color.ORANGE,
            Color.YELLOW,
            Color.YELLOWGREEN,
            Color.LIME,
            Color.GREEN,
            Color.DARKGREEN,
            Color.DARKTURQUOISE,
            Color.DEEPSKYBLUE,
            Color.AQUA,
            Color.AQUAMARINE,
            Color.BLUE,
            Color.MEDIUMPURPLE,
            Color.PURPLE
    };

    private final GameBoard gameBoard;

    private final double width;
    private final double height;

    /**
     * The column this block exists as in the grid
     */
    private final int x;

    /**
     * The row this block exists as in the grid
     */
    private final int y;

    /**
     * The value of this block (0 = empty, otherwise specifies the colour to render as)
     */
    private final IntegerProperty value = new SimpleIntegerProperty(0);


    private boolean cntr = false;
    private boolean hover;
    private Highlight t;

    /**
     * Create a new single Game Block
     * @param gameBoard the board this block belongs to
     * @param x the column the block exists in
     * @param y the row the block exists in
     * @param width the width of the canvas to render
     * @param height the height of the canvas to render
     */
    public GameBlock(GameBoard gameBoard, int x, int y, double width, double height) {
        this.gameBoard = gameBoard;
        this.width = width;
        this.height = height;
        this.x = x;
        this.y = y;

        //A canvas needs a fixed width and height
        setWidth(width);
        setHeight(height);

        //Do an initial paint
        paint();

        //When the value property is updated, call the internal updateValue method
        value.addListener(this::updateValue);
    }

    /**
     * When the value of this block is updated,
     * @param observable what was updated
     * @param oldValue the old value
     * @param newValue the new value
     */
    private void updateValue(ObservableValue<? extends Number> observable, Number oldValue, Number newValue) {
        paint();
    }

    /**
     * Handle painting of the block canvas
     */
    public void paint() {
        if (t != null) {
            t.stop();
            t = null;
        }
        //If the block is empty, paint as empty
        if(getValue() == 0) {
            paintEmpty();
        } else {
            //If the block is not empty, paint with the colour represented by the value
            paintColor(COLOURS[getValue()]);
        }
        //If the block has a centre, paint it
        if (cntr) {
            paintCentre();
        }
        //If you hover over a block, paint it
        if (hover) {
            paintHover();
        }
    }

    /**
     * Paint the centre of the current block
     */
    public void paintCentre() {
        var gc = getGraphicsContext2D();
        gc.setFill(Color.color(1.0, 1.0, 1.0, 0.5));
        gc.fillOval(width / 4.0, height / 4.0, width / 2.0, height / 2.0);
    }

    /**
     * Paint the current block when you hover on top of it
     */
    public void paintHover() {
        var gc = getGraphicsContext2D();
        gc.setFill(Color.color(1.0, 1.0, 1.0, 0.5));
        gc.fillRect(0.0, 0.0, width, height);
    }

    /**
     * Paint this canvas empty
     */
    private void paintEmpty() {
        var gc = getGraphicsContext2D();

        //Clear
        gc.clearRect(0,0,width,height);

        Color starter = Color.color(0.0D, 0.0D, 0.0D, 0.3D);
        Color finalcolor = Color.color(0.0D, 0.0D, 0.0D, 0.7D);

        //Fill
        gc.setFill(new LinearGradient(0.0, 0.0, 1.0, 1.0, true, CycleMethod.REFLECT, new Stop(0.0, starter), new Stop(1.0, finalcolor)));
        gc.fillRect(0,0, width, height);

        //Border
        gc.setStroke(Color.color(1.0, 1.0, 1.0, 0.5));
        gc.strokeRect(0,0,width,height);
    }

    /**
     * Paint this canvas with the given colour
     * @param colour the colour to paint
     */
    private void paintColor(Paint colour) {
        var gc = getGraphicsContext2D();

        //Clear
        gc.clearRect(0.0, 0.0, width, height);

        //Colour Fill
        gc.setFill(colour);
        gc.fillRect(0.0, 0.0, width, height);
        gc.setFill(Color.color(1.0, 1.0, 1.0, 0.1));
        gc.fillPolygon(new double[]{0.0, 0.0, width}, new double[]{0.0, height, height}, 3);
        gc.setFill(Color.color(1.0, 1.0, 1.0, 0.3));
        gc.fillRect(0.0, 0.0, width, 3.0);
        gc.setFill(Color.color(1.0, 1.0, 1.0, 0.3));
        gc.fillRect(0.0, 0.0, 3.0, height);
        gc.setFill(Color.color(0.0, 0.0, 0.0, 0.3));
        gc.fillRect(width - 3.0, 0.0, width, height);
        gc.setFill(Color.color(0.0, 0.0, 0.0, 0.3));
        gc.fillRect(0.0, height - 3.0, width, height);

        //Border
        gc.setStroke(Color.color(0.0, 0.0, 0.0, 0.5));
        gc.strokeRect(0.0, 0.0, width, height);
    }

    /**
     * Get the column of this block
     * @return column number
     */
    public int getX() {
        return x;
    }

    /**
     * Get the row of this block
     * @return row number
     */
    public int getY() {
        return y;
    }

    /**
     * Get the current value held by this block, representing it's colour
     * @return value
     */
    public int getValue() {
        return this.value.get();
    }

    /**
     * Bind the value of this block to another property. Used to link the visual block to a corresponding block in the Grid.
     * @param input property to bind the value to
     */
    public void bind(ObservableValue<? extends Number> input) {
        value.bind(input);
    }

    public void setCentre() {
        this.cntr = true;
        paint();
    }

    public void highlight() {
        t = new Highlight();
        t.start();
    }

    public void setHovering(boolean hover) {
        this.hover = hover;
        paint();
    }

    /**
     * Fade out functions are set, fades out to indicate a cleared block.
     * (useful to have it inside the GameBlock class so the functions are accessible)
     */
    public class Highlight extends AnimationTimer {
        double opc = 1.0D;

        public void handle(long n) {
            GameBlock.this.paintEmpty();
            opc -= 0.02;

            if (opc <= 0.0) {
                stop();
                GameBlock.this.t = null;
                return;
            }

            var gc = GameBlock.this.getGraphicsContext2D();
            gc.setFill(Color.color(0.0, 1.0, 0.0, opc));
            gc.fillRect(0.0, 0.0, GameBlock.this.width, GameBlock.this.height);
        }
    }
}
