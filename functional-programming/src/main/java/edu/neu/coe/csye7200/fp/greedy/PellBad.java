package edu.neu.coe.csye7200.fp.greedy;

public class PellBad {
    public PellBad() {
    }
    public long get(int n) {
        if (n < 0) throw new UnsupportedOperationException("Pell.get is not supported for negative n");
        if(n <= 1){
            return n;
        }
        long[] arr = new long[n + 1];
        arr[0] = 0l;
        arr[1] = 1l;
        for(int i = 2;i <= n;i++){
            arr[i] = 2 * arr[i - 1] + arr[i - 2];
        }
        return arr[n];
    }
}