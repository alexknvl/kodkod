package kodkod;

import scala.Unit;
import scala.reflect.ClassTag;
import scala.reflect.ClassTag$;

public class Utils {
    public static ClassTag<?> arrayClassTag(Object arr) {
        if (arr instanceof Unit[]) return ClassTag$.MODULE$.Unit();
        else if (arr instanceof byte[]) return ClassTag$.MODULE$.Byte();
        else if (arr instanceof short[]) return ClassTag$.MODULE$.Short();
        else if (arr instanceof int[]) return ClassTag$.MODULE$.Int();
        else if (arr instanceof long[]) return ClassTag$.MODULE$.Long();
        else if (arr instanceof float[]) return ClassTag$.MODULE$.Float();
        else if (arr instanceof double[]) return ClassTag$.MODULE$.Double();
        else if (arr instanceof Object[]) return ClassTag$.MODULE$.Any();
        else throw new UnsupportedOperationException();
    }

    public static int genericHashCode(Object arr, int start, int end, int init) {
        if (arr instanceof byte[]) return hashCode((byte[]) arr, start, end, init);
        else if (arr instanceof short[]) return hashCode((short[]) arr, start, end, init);
        else if (arr instanceof int[]) return hashCode((int[]) arr, start, end, init);
        else if (arr instanceof long[]) return hashCode((long[]) arr, start, end, init);
        else if (arr instanceof float[]) return hashCode((float[]) arr, start, end, init);
        else if (arr instanceof double[]) return hashCode((double[]) arr, start, end, init);
        else if (arr instanceof Object[]) return hashCode((Object[]) arr, start, end, init);
        else throw new UnsupportedOperationException();
    }

    public static int hashCode(long a[], int start, int end, int init) {
        if (a == null)
            return 0;

        int result = init;
        for (int i = start; i < end; i++) {
            long element = a[i];
            int elementHash = (int)(element ^ (element >>> 32));
            result = 31 * result + elementHash;
        }

        return result;
    }

    public static int hashCode(int a[], int start, int end, int init) {
        if (a == null)
            return 0;

        int result = init;
        for (int i = start; i < end; i++) {
            int element = a[i];
            result = 31 * result + element;
        }

        return result;
    }

    public static int hashCode(short a[], int start, int end, int init) {
        if (a == null)
            return 0;

        int result = init;
        for (int i = start; i < end; i++) {
            short element = a[i];
            result = 31 * result + element;
        }

        return result;
    }

    public static int hashCode(char a[], int start, int end, int init) {
        if (a == null)
            return 0;

        int result = init;
        for (int i = start; i < end; i++) {
            char element = a[i];
            result = 31 * result + element;
        }

        return result;
    }

    public static int hashCode(byte a[], int start, int end, int init) {
        if (a == null)
            return 0;

        int result = init;
        for (int i = start; i < end; i++) {
            byte element = a[i];
            result = 31 * result + element;
        }

        return result;
    }

    public static int hashCode(boolean a[], int start, int end, int init) {
        if (a == null)
            return 0;

        int result = init;
        for (int i = start; i < end; i++) {
            boolean element = a[i];
            result = 31 * result + (element ? 1231 : 1237);
        }

        return result;
    }

    public static int hashCode(float a[], int start, int end, int init) {
        if (a == null)
            return 0;

        int result = init;
        for (int i = start; i < end; i++) {
            float element = a[i];
            result = 31 * result + Float.floatToIntBits(element);
        }

        return result;
    }

    public static int hashCode(double a[], int start, int end, int init) {
        if (a == null)
            return 0;

        int result = init;
        for (int i = start; i < end; i++) {
            double element = a[i];
            long bits = Double.doubleToLongBits(element);
            result = 31 * result + (int)(bits ^ (bits >>> 32));
        }
        return result;
    }

    public static int hashCode(Object a[], int start, int end, int init) {
        if (a == null)
            return 0;

        int result = init;
        for (int i = start; i < end; i++) {
            Object element = a[i];
            result = 31 * result + (element == null ? 0 : element.hashCode());
        }

        return result;
    }
}
