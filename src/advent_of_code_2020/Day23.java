package advent_of_code;

public class Day23 {
  class Node {
    int label;
    Node next;

    Node(int label) {
      this.label = label;
      this.next = null;
    }

    @Override
    public String toString() {
      return "" + label; 
    }
  }

  class Cups {
    Node curr = null;
    Node pickup = null;
    int max = Integer.MIN_VALUE;
    int min = Integer.MAX_VALUE;

    Cups(int... labels) {
      Node last = null;
      for(int lb : labels) {
        max = Math.max(lb, max);
        min = Math.min(lb, min);

        Node n = new Node(lb);
        if (curr == null) {
          curr = n;
        }
        if (last != null) {
          last.next = n;
        }
        last = n;
      }

      last.next = curr;
    }

    private void onePickup() {
      Node pickupNode = curr.next;
      curr.next = pickupNode.next;
      pickupNode.next = null;

      if (pickup == null) {
        pickup = pickupNode;
      } else {
        Node n = pickup;
        pickup = pickupNode;
        pickup.next = n;
      }
    }

    public void pickup() {
      onePickup();
      onePickup();
      onePickup();
    }

    Node findCup(int label) {
      Node n = this.curr;
      do {
        if (n.label == label) {
          return n; 
        } 
        n = n.next;
      } while (n != this.curr);

      return null; 
    }

    Node findDest() {
      Node dest = null;
      int label = this.curr.label;
      while (dest == null) {
        label--;
        if (label < min) {
          label = max;
        }

        dest = findCup(label);
      }

      return dest;
    }


    void putdown() {
      Node dest = findDest();

      while(pickup != null) {
        // pop the first value from pickup 
        Node v = pickup;
        pickup = pickup.next;

        // insert v right after dest
        v.next = dest.next;
        dest.next = v;
      }
    }

    void updateCurrent() {
      curr = curr.next;
    }

    void step() {
      pickup();
      putdown();
      updateCurrent();
    }

    void doSteps(int count) {
      while(count > 0) {
        step();
        count--;

      }
    }

    @Override
    public String toString() {
      StringBuffer sb = new StringBuffer();
      sb.append("Min: " + min + "\n");
      sb.append("Max: " + max + "\n");
      sb.append("Cups: ");
      Node n = curr;
      do {
        sb.append(n);
        sb.append(" -> ");
        n = n.next;
      } while (n != curr);

      sb.append("\nPickup: ");
      n = pickup;
      while (n != null) {
        sb.append(n);
        sb.append(" -> ");
        n = n.next;
      }
      sb.append("null");

      return sb.toString();
    }
  }

  public void go() {
    long start = System.nanoTime();
    System.out.println("start");
    Cups cups = new Cups(6,5,3,4,2,7,9,1,8);
    System.out.println(cups);
    cups.doSteps(100);
    System.out.println(cups);
    System.out.println("end");
    long end = System.nanoTime();
    System.out.println((end - start) / 1000000);
  }

  public static void main(String[] args) {
    (new Day23()).go();
  }
}
