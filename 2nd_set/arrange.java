import java.io.*;
import java.util.*;

/* class Node {
    int value;
    Node left,right;

    node (int value){
        this.value = value;
        left = right = null;
    }
} */


public class Arrange{
    public static void main(String[] args) {
        String filename = args[0];
        String treeDescription = "";
        int node_count = 0;
        try {
            BufferedReader br = new BufferedReader(new FileReader(filename));
            node_count = Integer.parseInt(br.readLine());
            treeDescription = br.readLine();
            br.close();
        }
        catch (IOException e) {
            e.printStackTrace();
            return;
        }
        System.out.println("Node Count: " + node_count);
        System.out.println("Tree Description: " + treeDescription);
    }
}