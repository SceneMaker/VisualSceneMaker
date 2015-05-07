
/*
* To change this license header, choose License Headers in Project Properties.
* To change this template file, choose Tools | Templates
* and open the template in the editor.
 */
package de.dfki.vsm.editor.util.grid;

//~--- non-JDK imports --------------------------------------------------------

import org.ujmp.core.Matrix;
import org.ujmp.core.MatrixFactory;

//~--- JDK imports ------------------------------------------------------------

import java.util.ArrayList;

/**
 *
 * @author Souza Putra
 */
public class BezierFit {
    public BezierPoint[] bestFit(ArrayList<BezierPoint> points) {
        Matrix M = M();
        Matrix Minv;

        if (M.det() == 0) {
            Minv = M.invSPD();
        } else {
            Minv = M.inv();
        }

        Matrix U  = U(points);
        Matrix UT = U.transpose();
        Matrix X  = X(points);
        Matrix Y  = Y(points);
        Matrix A  = UT.mtimes(U);
        Matrix B;

        if (A.det() == 0) {
            B = A.invSPD();
        } else {
            B = A.inv();
        }

        Matrix        C = Minv.mtimes(B);
        Matrix        D = C.mtimes(UT);
        Matrix        E = D.mtimes(X);
        Matrix        F = D.mtimes(Y);
        BezierPoint[] P = new BezierPoint[4];

        for (int i = 0; i < 4; i++) {
            double      x = E.getAsDouble(i, 0);
            double      y = F.getAsDouble(i, 0);
            BezierPoint p = new BezierPoint(x, y);

            P[i] = p;
        }

        return P;
    }

    private Matrix Y(ArrayList<BezierPoint> points) {
        Matrix Y = MatrixFactory.fill(0.0, points.size(), 1);

        for (int i = 0; i < points.size(); i++) {
            Y.setAsDouble(points.get(i).getY(), i, 0);
        }

        return Y;
    }

    private Matrix X(ArrayList<BezierPoint> points) {
        Matrix X = MatrixFactory.fill(0.0, points.size(), 1);

        for (int i = 0; i < points.size(); i++) {
            X.setAsDouble(points.get(i).getX(), i, 0);
        }

        return X;
    }

    private Matrix U(ArrayList<BezierPoint> points) {
        double[] npls = normalizedPathLengths(points);
        Matrix   U    = MatrixFactory.fill(0.0, npls.length, 4);

        for (int i = 0; i < npls.length; i++) {
            U.setAsDouble(Math.pow(npls[i], 3), i, 0);
            U.setAsDouble(Math.pow(npls[i], 2), i, 1);
            U.setAsDouble(Math.pow(npls[i], 1), i, 2);
            U.setAsDouble(Math.pow(npls[i], 0), i, 3);
        }

        return U;
    }

    private Matrix M() {
        Matrix M = MatrixFactory.fill(0.0, 4, 4);

        M.setAsDouble(-1, 0, 0);
        M.setAsDouble(3, 0, 1);
        M.setAsDouble(-3, 0, 2);
        M.setAsDouble(1, 0, 3);
        M.setAsDouble(3, 1, 0);
        M.setAsDouble(-6, 1, 1);
        M.setAsDouble(3, 1, 2);
        M.setAsDouble(0, 1, 3);
        M.setAsDouble(-3, 2, 0);
        M.setAsDouble(3, 2, 1);
        M.setAsDouble(0, 2, 2);
        M.setAsDouble(0, 2, 3);
        M.setAsDouble(1, 3, 0);
        M.setAsDouble(0, 3, 1);
        M.setAsDouble(0, 3, 2);
        M.setAsDouble(0, 3, 3);

        return M;
    }

    /** Computes the percentage of path length at each point. Can directly be used as t-indices into the bezier curve. */
    private double[] normalizedPathLengths(ArrayList<BezierPoint> points) {
        double pathLength[] = new double[points.size()];

        pathLength[0] = 0;

        for (int i = 1; i < points.size(); i++) {
            BezierPoint p1       = points.get(i);
            BezierPoint p2       = points.get(i - 1);
            double      distance = Math.sqrt(Math.pow(p1.getX() - p2.getX(), 2) + Math.pow(p1.getY() - p2.getY(), 2));

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
        double      x1 = v1.getX();
        double      x2 = v2.getX();
        double      x3 = v3.getX();
        double      x4 = v4.getX();
        double      y1 = v1.getY();
        double      y2 = v2.getY();
        double      y3 = v3.getY();
        double      y4 = v4.getY();
        double      xt, yt;

        xt = x1 * Math.pow((1 - t), 3) + 3 * x2 * t * Math.pow((1 - t), 2) + 3 * x3 * Math.pow(t, 2) * (1 - t)
             + x4 * Math.pow(t, 3);
        yt = y1 * Math.pow((1 - t), 3) + 3 * y2 * t * Math.pow((1 - t), 2) + 3 * y3 * Math.pow(t, 2) * (1 - t)
             + y4 * Math.pow(t, 3);
        p = new BezierPoint(xt, yt);

        return p;
    }
}
