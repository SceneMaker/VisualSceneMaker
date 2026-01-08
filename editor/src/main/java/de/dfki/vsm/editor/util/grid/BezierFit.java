/*
* To change this license header, choose License Headers in Project Properties.
* To change this template file, choose Tools | Templates
* and open the template in the editor.
 */
package de.dfki.vsm.editor.util.grid;

//~--- JDK imports ------------------------------------------------------------

import java.util.ArrayList;

/**
 *
 * @author Souza Putra
 */
public class BezierFit {
    private static final double INVERT_EPS = 1e-10;

    public BezierPoint[] bestFit(ArrayList<BezierPoint> points) {
        double[][] m = m();
        double[][] minv = invert(m);

        double[][] u = u(points);
        double[][] ut = transpose(u);
        double[][] x = x(points);
        double[][] y = y(points);
        double[][] a = multiply(ut, u);
        double[][] binv = invert(a);
        double[][] c = multiply(minv, binv);
        double[][] d = multiply(c, ut);
        double[][] e = multiply(d, x);
        double[][] f = multiply(d, y);
        BezierPoint[] p = new BezierPoint[4];

        for (int i = 0; i < 4; i++) {
            double px = e[i][0];
            double py = f[i][0];
            p[i] = new BezierPoint(px, py);
        }

        return p;
    }

    private double[][] y(ArrayList<BezierPoint> points) {
        double[][] y = new double[points.size()][1];

        for (int i = 0; i < points.size(); i++) {
            y[i][0] = points.get(i).getY();
        }

        return y;
    }

    private double[][] x(ArrayList<BezierPoint> points) {
        double[][] x = new double[points.size()][1];

        for (int i = 0; i < points.size(); i++) {
            x[i][0] = points.get(i).getX();
        }

        return x;
    }

    private double[][] u(ArrayList<BezierPoint> points) {
        double[] npls = normalizedPathLengths(points);
        double[][] u = new double[npls.length][4];

        for (int i = 0; i < npls.length; i++) {
            double t = npls[i];
            u[i][0] = Math.pow(t, 3);
            u[i][1] = Math.pow(t, 2);
            u[i][2] = t;
            u[i][3] = 1.0;
        }

        return u;
    }

    private double[][] m() {
        double[][] m = new double[4][4];

        m[0][0] = -1;
        m[0][1] = 3;
        m[0][2] = -3;
        m[0][3] = 1;
        m[1][0] = 3;
        m[1][1] = -6;
        m[1][2] = 3;
        m[1][3] = 0;
        m[2][0] = -3;
        m[2][1] = 3;
        m[2][2] = 0;
        m[2][3] = 0;
        m[3][0] = 1;
        m[3][1] = 0;
        m[3][2] = 0;
        m[3][3] = 0;

        return m;
    }

    private double[][] transpose(double[][] input) {
        int rows = input.length;
        int cols = input[0].length;
        double[][] output = new double[cols][rows];

        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                output[c][r] = input[r][c];
            }
        }

        return output;
    }

    private double[][] multiply(double[][] left, double[][] right) {
        int rows = left.length;
        int cols = right[0].length;
        int shared = right.length;
        double[][] result = new double[rows][cols];

        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                double sum = 0.0;
                for (int k = 0; k < shared; k++) {
                    sum += left[r][k] * right[k][c];
                }
                result[r][c] = sum;
            }
        }

        return result;
    }

    private double[][] invert(double[][] input) {
        int n = input.length;
        double[][] a = new double[n][n];
        double[][] inv = new double[n][n];

        for (int r = 0; r < n; r++) {
            System.arraycopy(input[r], 0, a[r], 0, n);
            inv[r][r] = 1.0;
        }

        for (int i = 0; i < n; i++) {
            int pivot = i;
            double max = Math.abs(a[i][i]);
            for (int r = i + 1; r < n; r++) {
                double value = Math.abs(a[r][i]);
                if (value > max) {
                    max = value;
                    pivot = r;
                }
            }

            if (max < INVERT_EPS) {
                a[i][i] = a[i][i] >= 0 ? INVERT_EPS : -INVERT_EPS;
            } else if (pivot != i) {
                swapRows(a, i, pivot);
                swapRows(inv, i, pivot);
            }

            double diag = a[i][i];
            if (Math.abs(diag) < INVERT_EPS) {
                diag = diag >= 0 ? INVERT_EPS : -INVERT_EPS;
                a[i][i] = diag;
            }

            double invDiag = 1.0 / diag;
            for (int c = 0; c < n; c++) {
                a[i][c] *= invDiag;
                inv[i][c] *= invDiag;
            }

            for (int r = 0; r < n; r++) {
                if (r == i) {
                    continue;
                }
                double factor = a[r][i];
                if (factor == 0.0) {
                    continue;
                }
                for (int c = 0; c < n; c++) {
                    a[r][c] -= factor * a[i][c];
                    inv[r][c] -= factor * inv[i][c];
                }
            }
        }

        return inv;
    }

    private void swapRows(double[][] matrix, int a, int b) {
        double[] temp = matrix[a];
        matrix[a] = matrix[b];
        matrix[b] = temp;
    }

    /** Computes the percentage of path length at each point. Can directly be used as t-indices into the bezier curve. */
    private double[] normalizedPathLengths(ArrayList<BezierPoint> points) {
        double[] pathLength = new double[points.size()];

        pathLength[0] = 0;

        for (int i = 1; i < points.size(); i++) {
            BezierPoint p1 = points.get(i);
            BezierPoint p2 = points.get(i - 1);
            double distance = Math.sqrt(Math.pow(p1.getX() - p2.getX(), 2) + Math.pow(p1.getY() - p2.getY(), 2));

            pathLength[i] += pathLength[i - 1] + distance;
        }

        double[] zpl = new double[pathLength.length];

        for (int i = 0; i < zpl.length; i++) {
            zpl[i] = pathLength[i] / pathLength[pathLength.length - 1];
        }

        return zpl;
    }

    /**
     * Computes b(t).
     * @param t
     * @param v1
     * @param v2
     * @param v3
     * @param v4
     * @return
     */
    private BezierPoint pointOnCurve(double t, BezierPoint v1, BezierPoint v2, BezierPoint v3, BezierPoint v4) {
        BezierPoint p;
        double x1 = v1.getX();
        double x2 = v2.getX();
        double x3 = v3.getX();
        double x4 = v4.getX();
        double y1 = v1.getY();
        double y2 = v2.getY();
        double y3 = v3.getY();
        double y4 = v4.getY();
        double xt;
        double yt;

        xt = x1 * Math.pow((1 - t), 3) + 3 * x2 * t * Math.pow((1 - t), 2) + 3 * x3 * Math.pow(t, 2) * (1 - t)
             + x4 * Math.pow(t, 3);
        yt = y1 * Math.pow((1 - t), 3) + 3 * y2 * t * Math.pow((1 - t), 2) + 3 * y3 * Math.pow(t, 2) * (1 - t)
             + y4 * Math.pow(t, 3);
        p = new BezierPoint(xt, yt);

        return p;
    }
}
