
// package org.opensourcephysics.manual.ch08;
import org.opensourcephysics.display.DrawingFrame;
import org.opensourcephysics.display.DrawingPanel;
import org.opensourcephysics.display2d.ArrayData;
import org.opensourcephysics.display2d.SurfacePlot;
import org.opensourcephysics.display2d.SurfacePlotMouseController;
import javax.swing.JFrame;

/**
 * SurfacePlotApp creates a surface plot.
 *
 * @author       Wolfgang Christian
 * @version 1.0
 */
public class SurfaceArrayApp {

  /**
   * Starts the Java application.
   * @param args[]  the input parameters
   */
  public static void main(String[] args) {
    DrawingPanel drawingPanel = new DrawingPanel();
    drawingPanel.setShowCoordinates(false);
    DrawingFrame frame = new DrawingFrame(drawingPanel);
    ArrayData arraydata = new ArrayData(32, 32, 1);
    arraydata.setScale(-1, 1, -1, 1);
    double[][] data = arraydata.getData()[0];
    double x = arraydata.getLeft(), dx = arraydata.getDx();
    for(int i = 0, nx = data.length;i<nx;i++) {
      double y = arraydata.getTop(), dy = arraydata.getDy();
      for(int j = 0, ny = data[0].length;j<ny;j++) {
        data[i][j] = y*x; // magnitude
        y += dy;
      }
      x += dx;
    }
    SurfacePlot plot = new SurfacePlot(arraydata);
    drawingPanel.addDrawable(plot);
    SurfacePlotMouseController mouseController = new SurfacePlotMouseController(drawingPanel, plot);
    drawingPanel.addMouseListener(mouseController);
    drawingPanel.addMouseMotionListener(mouseController);
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.setVisible(true);
    // GUIUtils.timingTest(plot);
  }
}


