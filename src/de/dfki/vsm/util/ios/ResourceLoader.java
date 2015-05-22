package de.dfki.vsm.util.ios;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.log.LOGDefaultLogger;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Image;
import java.awt.Toolkit;

import java.net.URL;

import javax.swing.ImageIcon;

/**
 * A utility class for loading objects from JAR files.
 *
 * @author Peter Adolphs
 */
public class ResourceLoader {
    private final static LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

//  public static File loadFile(String resource) {
//      return loadFile(ResourceLoader.class, resource);
//  }
//
//  public static File loadFile(Class<?> base, String resource) {
//      File file = null;
//
//      URL url = base.getResource(resource);
//     System.err.println(url.toString());
//      //URI uri = base.getResource(resource);
//      if (url != null) {
//          try {
//              file = new File(url.toURI());
//          } catch (URISyntaxException e) {
//              e.printStackTrace();
//          }
//      } else {
//          mLogger.warning(String.format("Could not find resource '%s'.", resource));
//      }
//      return file;
//  }

    /**
     * Load an image icon from the specified resource which is resolved relative
     * to this class.
     *
     * @param resource the resource to load
     * @return an image icon
     */
    public static ImageIcon loadImageIcon(String resource) {
        return loadImageIcon(ResourceLoader.class, resource);
    }

    /**
     * Load an image icon from the specified resource which is resolved relative
     * to the specified base class.
     *
     * @param base the base class against which the resource is resolved
     * @param resource the resource to load
     * @return an image icon
     */
    public static ImageIcon loadImageIcon(Class<?> base, String resource) {
        ImageIcon icon = null;
        URL       url  = base.getResource(resource);

        if (url != null) {
            icon = new ImageIcon(url);
        } else {
            mLogger.warning(String.format("Could not find resource '%s'.", resource));
        }

        return icon;
    }

    /**
     * Load an image from the specified resource which is resolved relative to
     * this class.
     *
     * @param resource the resource to load
     * @return an image icon
     */
    public static Image loadImage(String resource) {
        return loadImage(ResourceLoader.class, resource);
    }

    /**
     * Load an image from the specified resource which is resolved relative to
     * the specified base class.
     *
     * @param base the base class against which the resource is resolved
     * @param resource the resource to load
     * @return an image icon
     */
    public static Image loadImage(Class<?> base, String resource) {
        Image image = null;
        URL   url   = base.getResource(resource);

        if (url != null) {
            image = Toolkit.getDefaultToolkit().createImage(url);
        } else {
            mLogger.warning(String.format("Could not find resource '%s'.", resource));
        }

        return image;
    }
}
