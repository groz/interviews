/**
 * Find contiguous subarray with the largest sum.
 */

public class MaxSubarrayJava {

    static int maxSubarray(int[] arr) {
        int currentSum = 0, maxSum = 0;

        for (int a : arr) {
            currentSum = Math.max(currentSum + a, 0);
            maxSum = Math.max(maxSum, currentSum);
        }

        return maxSum;
    }

    public static void main(String[] args) {
        int[] arr = { 1, -2, 3, 5, -4, 5 };
        System.out.println(maxSubarray(arr));
    }
}
