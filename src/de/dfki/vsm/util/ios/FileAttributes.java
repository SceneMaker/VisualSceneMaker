package de.dfki.vsm.util.ios;

//~--- JDK imports ------------------------------------------------------------

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

/**
 * @author Not me
 */
public class FileAttributes {
    public static String relPath(String relPath) {
        String userDir = System.getProperty("user.dir").trim();

        if (relPath.startsWith(userDir)) {
            return relPath.substring(userDir.length() + 1);
        } else {
            return relPath;
        }
    }

    public static String adaptSeparator(String path) {
        String currentSystemSeparator = System.getProperty("file.separator");

        // do something if given path does not show a current path separator
        if (!path.contains(currentSystemSeparator)) {
            if (currentSystemSeparator.equalsIgnoreCase("/")) {
                return path.replace("\\", currentSystemSeparator);
            } else {
                return path.replace("/", currentSystemSeparator);
            }
        } else {
            return path;
        }
    }

    public static boolean compare(File one, File two) throws IOException {
        if (one.length() == two.length()) {
            FileInputStream fis1 = new FileInputStream(one);
            FileInputStream fis2 = new FileInputStream(two);

            try {
                int temp = 0;

                while ((temp = fis1.read()) != -1) {
                    if (temp != fis2.read()) {
                        return false;
                    }
                }

                return true;
            } finally {
                fis1.close();
                fis2.close();
            }
        }

        return false;
    }

    public static String getExtension(File fileName) {
        String str = fileName.getName();
        int    pos = str.lastIndexOf('.');

        if (pos > -1) {
            return str.substring(pos + 1);
        } else {
            return "";
        }
    }
}
