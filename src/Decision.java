import java.util.function.Function;

interface Func<T> {
    T apply(double x, double y, double z);
}

class GaussMethod {
    public static double[] main(double[][] mat, double[] bValues) {

        /* Ввод данных */
        double[][] A = new double[3][3];
        double[] b = new double[3];
        for (int i = 0; i < 3; i++) {
            System.arraycopy(mat[i], 0, A[i], 0, 3);
        }
        System.arraycopy(bValues, 0, b, 0, 3);

        /* Метод Гаусса */

        int N = 3;
        for (int p = 0; p < N; p++) {

            int max = p;
            for (int i = p + 1; i < N; i++) {
                if (Math.abs(A[i][p]) > Math.abs(A[max][p])) {
                    max = i;
                }
            }
            double[] temp = A[p];
            A[p] = A[max];
            A[max] = temp;
            double t = b[p];
            b[p] = b[max];
            b[max] = t;

            if (Math.abs(A[p][p]) <= 1e-10) {
                System.out.println("NO");
                System.exit(-1);
            }

            for (int i = p + 1; i < N; i++) {
                double alpha = A[i][p] / A[p][p];
                b[i] -= alpha * b[p];
                for (int j = p; j < N; j++) {
                    A[i][j] -= alpha * A[p][j];
                }
            }
        }

        // Обратный проход

        double[] x = new double[N];
        for (int i = N - 1; i >= 0; i--) {
            double sum = 0.0;
            for (int j = i + 1; j < N; j++) {
                sum += A[i][j] * x[j];
            }
            x[i] = (b[i] - sum) / A[i][i];
        }

        return x;


    }
}

class GaussMeth {
    public static final double EPS = 1E-5;

    public static void Swap_Lines(int k1, int k2, int n, Double[][] A, Boolean[] mark) {
        for (int j = 0; j < n; j++) {
            Double tmp;
            tmp = A[k1][j];
            A[k1][j] = A[k2][j];
            A[k2][j] = tmp;
        }
        Boolean tmp;
        tmp = mark[k1];
        mark[k1] = mark[k2];
        mark[k2] = tmp;
    }

    public static double[] getRoot(double[][] mas) {
        int m = 3;
        int n = 3;

        Double[][] A = new Double[m][n + 1];
        double[] answer = new double[n];

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n + 1; j++) {
                A[i][j] = mas[i][j];
            }
        }

        int min_size = 3;

        for (int k = 0; k < min_size; k++) {
            double maxv = 0;
            int position_of_line_with_maxv = k;
            for (int i = k; i < m; i++) {
                if (Math.abs(A[i][k]) > maxv) {
                    maxv = Math.abs(A[i][k]);
                    position_of_line_with_maxv = i;
                }
            }
            for (int j = 0; j < n + 1; j++) {
                double tmp = A[k][j];
                A[k][j] = A[position_of_line_with_maxv][j];
                A[position_of_line_with_maxv][j] = tmp;
            }

            if (Math.abs(maxv) < EPS) {
                continue;
            }

            for (int i = 0; i < m; i++) {
                if (i == k) continue;

                double multiplier = A[i][k] / A[k][k];
                for (int j = k; j < n + 1; j++) {
                    A[i][j] -= multiplier * A[k][j];
                }
            }
        }

        for (int k = 0; k < min_size; k++) {
            if (Math.abs(A[k][k]) > EPS) {
                double multiplier = A[k][k];
                if (Math.abs(multiplier) < EPS) continue;
                for (int j = k; j < n + 1; j++) {
                    A[k][j] /= multiplier;
                }
            }
        }

        Boolean[] mark = new Boolean[m];
        for (int i = 0; i < m; i++) {
            mark[i] = Boolean.FALSE;
        }

        for (int k1 = 0; k1 < m; k1++) {
            if (mark[k1] == Boolean.TRUE) continue;
            for (int k2 = k1 + 1; k2 < m; k2++) {
                boolean is_equal = true;
                for (int j = 0; j < n + 1; j++) {
                    if (Math.abs(A[k1][j] - A[k2][j]) > EPS) {
                        is_equal = false;
                        break;
                    }
                }
                if (is_equal) {
                    mark[k2] = true;
                }
            }
        }
        for (int i = 0; i < m; i++) {
            int cnt_of_zeroes = 0;
            for (int j = 0; j < n + 1; j++) {
                if (Math.abs(A[i][j]) < EPS) {
                    cnt_of_zeroes++;
                    A[i][j] = 0.0;
                }
            }
            if (cnt_of_zeroes == n + 1) {
                mark[i] = Boolean.TRUE;
            }
            if (cnt_of_zeroes == n && Math.abs(A[i][n]) > EPS) {
                System.out.println("The system of equations is inconsistent");
                System.exit(-1);
            }
        }

        for (int i = 0; i < m; i++) {
            for (int j = i + 1; j < m; j++) {
                if (mark[i] == Boolean.TRUE && mark[j] == Boolean.FALSE) {
                    Swap_Lines(i, j, n, A, mark);
                }
            }
        }

        /*  for (int i = 0; i < m; i++) {
            for (int j = 0; j < n+1; j++) {
                System.out.print(A[i][j] + " ");
            }
            System.out.println();
        }*/

        int cnt_of_marks = 0;
        for (int i = 0; i < m; i++) {
            if (mark[i] == Boolean.TRUE) cnt_of_marks++;
        }
        int bottom_border = m - 1 - cnt_of_marks;

        if (bottom_border == n - 1) {
            for (int k = n - 1; k >= 0; k--) {
                answer[k] = A[k][n] / A[k][k];
            }

            //System.out.println("Answer:");
            for (int k = 0; k < n - 1; k++) {
                System.out.print(answer[k] + " ");
            }
            //System.out.println(answer[n-1]);
        } else {
            int cnt_of_free_variables = n - (bottom_border + 1);

            Boolean[] marked_variables = new Boolean[n];
            for (int i = 0; i < n; i++) {
                marked_variables[i] = Boolean.FALSE;
            }

            for (int j = 0; j < n; j++) {
                int cnt_of_zeroes = 0;
                for (int i = 0; i < bottom_border; i++) {
                    if (Math.abs(A[i][j]) < EPS) {
                        cnt_of_zeroes++;
                    }
                }
                if (cnt_of_zeroes == bottom_border + 1) {
                    if (cnt_of_free_variables > 0) {
                        marked_variables[j] = Boolean.TRUE;
                        cnt_of_free_variables--;
                    }
                }
            }
            for (int i = n - 1; i >= 0; i--) {
                if (cnt_of_free_variables == 0) break;
                marked_variables[i] = Boolean.TRUE;
                cnt_of_free_variables--;
            }
            //System.out.println("Initialization of free variables:");
            for (int i = 0; i < n; i++) {
                if (marked_variables[i] == Boolean.TRUE) {
                    answer[i] = 1.0;
                    //System.out.println("Let: " + i + "-th variable assigned: 1.0" );
                }
            }
            //System.out.println("Answer:");
            /*for (int i = 0; i < n; i++) {
                if (marked_variables[i] == Boolean.TRUE) {
                    //System.out.println(i+"-th variable is free");
                }
            }*/

            for (int i = bottom_border; i >= 0; i--) {
                double cur_sum = 0;

                int cur_variable = 0;
                for (int j = 0; j < n; j++) {
                    if (marked_variables[j] == Boolean.FALSE && Math.abs(A[i][j]) > EPS) {
                        cur_variable = j;
                        break;
                    }
                }

                //System.out.print("X[" + cur_variable + "] = ");
                for (int j = 0; j < n; j++) {
                    if (marked_variables[j] == Boolean.TRUE) {
                        cur_sum += answer[j] * A[i][j];
                        //System.out.print("(" + -A[i][j] + "/" + A[i][cur_variable] + ")" + "*X[" + j + "] ");
                    }
                }
                //System.out.println();

                cur_sum *= -1;
                cur_sum += A[i][n];


                for (int j = 0; j < n; j++) {
                    if (marked_variables[j] == Boolean.FALSE && Math.abs(A[i][j]) > EPS) {
                        answer[j] = cur_sum / A[i][j];
                        marked_variables[j] = Boolean.TRUE;
                        break;
                    }
                }

            }

/*
            for (int i = 0; i < n; i++) {
                if (Math.abs(answer[i]) < EPS) answer[i] = 0.0;
            }
*/

            //System.out.println("One of the solutions:");
        }
        return answer;
    }
}

public class Decision {

    public static double[] getDeltas(Double derX1, Double derX2, Double derX3, Double derY1, Double derY2, Double derY3,
                                     Double derZ1, Double derZ2, Double derZ3, Double eq1, Double eq2, Double eq3) {
        double[][] mas = new double[3][3];
        double[] bValues = new double[3];
        mas[0][0] = derX1;
        mas[0][1] = derY1;
        mas[0][2] = derZ1;
        mas[1][0] = derX2;
        mas[1][1] = derY2;
        mas[1][2] = derZ2;
        mas[2][0] = derX3;
        mas[2][1] = derY3;
        mas[2][2] = derZ3;
        bValues[0] = -eq1;
        bValues[1] = -eq2;
        bValues[2] = -eq3;

        return GaussMethod.main(mas, bValues);
    }

    public static double[] findRoot(Func<Double> eq1, Func<Double> eq2, Func<Double> eq3,
                                    Func<Double> derX1, Func<Double> derX2, Func<Double> derX3,
                                    Func<Double> derY1, Func<Double> derY2, Func<Double> derY3,
                                    Func<Double> derZ1, Func<Double> derZ2, Func<Double> derZ3,
                                    double x, double y, double z, double epsilon) {


        double newX = x;
        double newY = y;
        double newZ = z;
        System.out.printf("Initial values: x = %.4f ; y = %.4f, z = %.4f", newX, newY, newZ);
        System.out.println();
        int tmp = 0;
        while (true) {
            double[] deltas = getDeltas(derX1.apply(newX, newY, newZ), derX2.apply(newX, newY, newZ), derX3.apply(newX, newY, newZ),
                    derY1.apply(newX, newY, newZ), derY2.apply(newX, newY, newZ), derY3.apply(newX, newY, newZ),
                    derZ1.apply(newX, newY, newZ), derZ2.apply(newX, newY, newZ), derZ3.apply(newX, newY, newZ),
                    eq1.apply(newX, newY, newZ), eq2.apply(newX, newY, newZ), eq3.apply(newX, newY, newZ));

            if ((Math.abs(deltas[0]) < epsilon) && (Math.abs(deltas[1]) < epsilon) && (Math.abs(deltas[2]) < epsilon)) {
                System.out.printf("%d. DELTAS: d1 = %.4f ; d2 = %.4f, d3 = %.4f", tmp + 1, deltas[0], deltas[1], deltas[2]);
                System.out.printf("%d. values: x = %.4f ; y = %.4f, z = %.4f", ++tmp, newX + deltas[0], newY + deltas[1]
                        , newZ + deltas[2]);
                System.out.println();
                return new double[]{newX + deltas[0], newY + deltas[1], newZ + deltas[2]};
            }
            newX += deltas[0];
            newY += deltas[1];
            newZ += deltas[2];
            /*if (tmp < 10) {
                System.out.printf("%d. DELTAS: d1 = %.4f ; d2 = %.4f, d3 = %.4f", tmp + 1, deltas[0], deltas[1], deltas[2]);
                System.out.printf("%d. values: x = %.4f ; y = %.4f, z = %.4f", ++tmp, newX, newY, newZ);
                System.out.println();
            } else {
                System.exit(0);
            }*/
            System.out.printf("%d. DELTAS: d1 = %.4f ; d2 = %.4f, d3 = %.4f", tmp + 1, deltas[0], deltas[1], deltas[2]);
            System.out.printf("%d. values: x = %.4f ; y = %.4f, z = %.4f", ++tmp, newX, newY, newZ);
            System.out.println();



        }

    }

    public static void main(String[] args) {

        Func<Double> equation1 = (x, y, z) -> Math.pow(x, 3) + Math.pow(y, 3) + Math.pow(z, 3) - 1.728;
        Func<Double> equation2 = (x, y, z) -> 1.5 * x * y - 3.7 * Math.pow(x, 2) * Math.pow(y, 2) + 7.2 + 2.63232;
        Func<Double> equation3 = (x, y, z) -> 10.2 * Math.pow(x, 2) - 1.7 * Math.pow(y, 2) - 0.7 - 11.54;
        Func<Double> derivativeX1 = (x, y, z) -> 3 * Math.pow(x, 2);
        Func<Double> derivativeX2 = (x, y, z) -> 1.5 * y - 7.4 * x * Math.pow(y, 2);
        Func<Double> derivativeX3 = (x, y, z) -> 20.4 * x;
        Func<Double> derivativeY1 = (x, y, z) -> 3 * Math.pow(y, 2);
        Func<Double> derivativeY2 = (x, y, z) -> 1.5 * x - 7.4 * Math.pow(x, 2) * y;
        Func<Double> derivativeY3 = (x, y, z) -> (-1) * 3.4 * y;
        Func<Double> derivativeZ1 = (x, y, z) -> 3 * Math.pow(z, 2);
        Func<Double> derivativeZ2 = (x, y, z) -> 0D;
        Func<Double> derivativeZ3 = (x, y, z) -> 0D;


        double x = 0.5;
        double y = -0.5;
        double z = 0.2;
        double err = 0.01;
        double[] roots = findRoot(equation1, equation2, equation3, derivativeX1, derivativeX2, derivativeX3,
                derivativeY1, derivativeY2, derivativeY3, derivativeZ1, derivativeZ2, derivativeZ3,
                x, y, z, err);
        x = roots[0];
        y = roots[1];
        z = roots[2];

        System.out.println("-".repeat(30));
        System.out.println("-".repeat(30));
        System.out.println("-".repeat(30));
        System.out.printf("ROOTS! x = %.4f, y = %.4f, z = %.4f", x, y, z);
        System.out.println();
        System.out.println("-".repeat(30));
        System.out.printf("ROOTS IN FUNCTIONS: f1 = %.4f, f2 = %.4f, f3 = %.4f", equation1.apply(x, y, z),
                equation2.apply(x, y, z),
                equation3.apply(x, y, z));

    }
}