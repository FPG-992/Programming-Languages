// Exume 1<=N<=30 koutia
// to kathe kouti exei 1<=mi<=42 volume space
// xrisimopoioume to idio oses fores theloume
// Max Ogkos = 1<=M<=42
// theloume olous tous pithanous sundiasmous koutiwn pou na gemisun akrivos ogko = M

//goal: vriskoume olous tous sundiasmous pou na gemizoun akrivos ogko M
//tous kanoume print
//an den einai efikto print "IMPOSSIBLE"

import java.io.*;
import java.util.*;

public class Boxes {
    private static boolean found = false;

    public static void main(String[] args) throws Exception{
        Scanner sc = new Scanner(new File(args[0]));
        int N = sc.nextInt(); // number of boxes
        int M = sc.nextInt(); // max volume
        int[] sizes = new int[N];
        for (int i = 0; i < N; i++) {
            sizes[i] = sc.nextInt(); // volume of each box
        }
        sc.close();

        Arrays.sort(sizes);

        dfsPrint(sizes, 0, M, new ArrayList<>());

        if (!found) {
            System.out.println("IMPOSSIBLE");
        }

    }

    private static void dfsPrint(int[] sizes, int start, int remaining, List<Integer> current){
        if (remaining == 0){
            found = true;
            for (int i =0; i < current.size(); i++) {
                if (i > 0) {
                    System.out.print(' ');
                }
                System.out.print(current.get(i));
            }
                System.out.println();
                return;
        }

        for (int i = start; i < sizes.length; i++) {
            int v = sizes[i];
            if (v > remaining) {
                break;
            }
            current.add(v);
            dfsPrint(sizes, i, remaining - v, current);
            current.remove(current.size() - 1);
        }
    }
}