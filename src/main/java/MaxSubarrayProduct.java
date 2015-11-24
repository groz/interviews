/**
 * Find maximum product in the subarray of a given one.
 * Examples:
 * Input: [2, 3, -4, 5] Output: 6
 * Input: [2, 3, -4, 5, -1] Output: 120
 */

class MaxSubarrayProduct {

    // Solution assumes there exists a positive answer
    static int maxProduct(int[] arr) {

        int pos = 1, neg = 1, max = 1;

        for (int a : arr) {
            if (a > 0) {
                pos *= a;
                neg *= a;
            } else if (a == 0) {
                pos = 1;
                neg = 1;
            } else {
                pos = Math.max(neg * a, 1);
                neg = Math.min(neg * a, a);
            }

            max = Math.max(max, pos);
        }

        return max;
    }

    public static void main(String[] args) {
        System.out.println(maxProduct(new int[]{2, 3, -4, 5}));
        System.out.println(maxProduct(new int[]{2, 3, -4, 5, -1}));
        System.out.println(maxProduct(new int[]{2, 3, 0, -4, 5, -1}));

        System.out.println(maxProduct(new int[]{-1, -2, -3}));
        System.out.println(maxProduct(new int[]{-3, -2, -1}));

        System.out.println(maxProduct(new int[]{6, -3, -10, 0, 2}));
        System.out.println(maxProduct(new int[]{-1, -3, -10, 0, 60}));
        System.out.println(maxProduct(new int[]{-2, -3, 0, -2, -40}));
    }
}
